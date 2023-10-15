import type * as es from 'estree'
import { posix as posixPath } from 'path';

import { Context } from "../..";
import { parse } from '../../parser/parser';
import assert from '../../utils/assert';
import { extractIdsFromPattern } from '../../utils/ast/helpers';
import { isModuleDeclaration, isSourceModule, isVariableDeclaration } from '../../utils/ast/typeGuards';
import { ModuleNotFoundError, UndefinedDefaultImportError, UndefinedImportError, UndefinedNamespaceImportError } from '../errors';
import { memoizedGetModuleDocsAsync } from '../moduleLoaderAsync';
import { DirectedGraph } from "./directedGraph";
import { defaultResolutionOptions, ResolutionOptions, resolveFile } from "./resolver";

class PreprocessError extends Error {}

type ModuleDeclarationWithSource = Exclude<es.ModuleDeclaration, es.ExportDefaultDeclaration>

type PotentialModuleDocs = {
  has: (key: string) => boolean 
  size: number
  [Symbol.iterator](): Iterator<string>
}

export type ModuleAnalysisOptions = {
  resolverOptions: ResolutionOptions
  shouldAddFileName: boolean
  allowUndefinedImports: boolean
}

export const defaultAnalysisOptions: ModuleAnalysisOptions = {
  resolverOptions: defaultResolutionOptions,
  shouldAddFileName: false,
  allowUndefinedImports: false
}

export default async function parseProgramsAndConstructImportGraph(
  fileGetter: (path: string) => Promise<string | undefined>,
  entrypointFilePath: string,
  context: Context,
  options: ModuleAnalysisOptions
) {
  const importGraph = new DirectedGraph()
  const programs: Record<string, es.Program> = {}
  const moduleDocs: Record<string, Set<string>> = {}

  function resolveFileWrapper(file: string) {
    return resolveFile(file, async (str) => {
      const file = await fileGetter(str);
      return file !== undefined;
    }, options.resolverOptions);
  }

  async function resolveAndParseFile(fromModule: string, node: ModuleDeclarationWithSource): Promise<[string, PotentialModuleDocs]> {
    const relDstPath = node.source?.value
    assert(typeof relDstPath === 'string', `Expected module declaration to be of type string, got ${node.source?.value}`)

    const [absDstPath, resolved] = await resolveFileWrapper(relDstPath) 

    if (!resolved) {
      throw new ModuleNotFoundError(absDstPath, node)
    }

    if (importGraph.hasEdge(fromModule, absDstPath)) {
      // If we've seen this edge before, then we must have a cycle
      // so exit early and proceed to locate the cycle
      throw new PreprocessError()
    }

    // We can always assume that Source modules have to be loaded
    // first, so we don't need to add these to the import graph
    if (!isSourceModule(absDstPath)) {
      importGraph.addEdge(fromModule, absDstPath);
      await enumerateModuleDecls(absDstPath)

      node.source!.value = absDstPath;
      return [absDstPath, moduleDocs[absDstPath]] 
    }

    if (options.allowUndefinedImports) {
      return [absDstPath, {
          has: () => true,
          size: 1000,
          [Symbol.iterator]: () => ({
            next: () => ({ 
              done: true,
              value: ''
            })
          })
        }
      ]
    }
    const docs = await memoizedGetModuleDocsAsync(absDstPath);
    if (!docs) {
      throw new Error()
    } 

    return [absDstPath, new Set(Object.keys(docs))]
  }

  async function enumerateModuleDecls(sourceModule: string) {
    // No need to parse programs we've already parsed before
    // or if the module is a source module
    if (isSourceModule(sourceModule) || sourceModule in programs) return;
    assert(posixPath.isAbsolute(sourceModule), 'parseFile should only be used with absolute paths')
    
    const fileText = await fileGetter(sourceModule);
    assert(!!fileText, 'If the file does not exist, it should\'ve been caught')
    const parseOptions = options.shouldAddFileName ? {
      sourceFile: sourceModule,
    } : {}

    const program = parse(fileText, context, parseOptions)
    if (!program) {
      // The program has syntax errors or something,
      // exit early
      throw new PreprocessError()
    }
    programs[sourceModule] = program
    moduleDocs[sourceModule] = new Set()

    for (const node of program.body) {
      if (node.type === 'ExportDefaultDeclaration') {
        if (!options.allowUndefinedImports) {
          assert(!moduleDocs[sourceModule].has('default'), 'Multiple default exports should\'ve been caught by the parser')
          moduleDocs[sourceModule].add('default')
        }
        continue
      } else if (node.type === 'ExportNamedDeclaration') {
        if (node.declaration) {
          const ids = isVariableDeclaration(node.declaration) ? node.declaration.declarations.flatMap(({ id }) => extractIdsFromPattern(id)) : [
            node.declaration.id!
          ]
          
          ids.forEach(({ name }) => moduleDocs[sourceModule].add(name))
          continue;
        }

        for (const spec of node.specifiers) {
          moduleDocs[sourceModule].add(spec.exported.name)
        }

        if (!node.source) continue
      } else if (!isModuleDeclaration(node)) {
        continue
      }


      const [dstModule, dstModuleDocs] = await resolveAndParseFile(sourceModule, node)

      if (options.allowUndefinedImports) continue

      switch (node.type) {
        case 'ExportNamedDeclaration':
        case 'ImportDeclaration': {
          for (const spec of node.specifiers) {
            if (spec.type === 'ImportNamespaceSpecifier') {
              if (dstModuleDocs.size === 0) {
                throw new UndefinedNamespaceImportError(dstModule, spec)
              }
              continue
            }

            let importedName: string
            switch (spec.type) {
              case 'ImportSpecifier': {
                importedName = spec.imported.name
                break
              }
              case 'ImportDefaultSpecifier': {
                importedName = 'default'
                break
              }
              case 'ExportSpecifier': {
                importedName = spec.local.name
                break
              }
            }

            if (!dstModuleDocs.has(importedName)) {
              if (importedName === 'default') {
                throw new UndefinedDefaultImportError(dstModule, spec)
              }
              throw new UndefinedImportError(importedName, dstModule, spec)
            }
          }
          break
        }
        case 'ExportAllDeclaration': {
          if (dstModuleDocs.size === 0) {
            throw new UndefinedNamespaceImportError(dstModule, node)
          }

          if (node.exported) {
            moduleDocs[sourceModule].add(node.exported.name)
          } else {
            for (const each of dstModuleDocs) {
              moduleDocs[sourceModule].add(each)
            }
          }
          break
        }
      }
    }
  }

  try {
    const [entrypointAbsPath, entrypointResolved] = await resolveFileWrapper(entrypointFilePath);
    if (!entrypointResolved) {
      throw new ModuleNotFoundError(entrypointAbsPath);
    }
    await enumerateModuleDecls(entrypointAbsPath)
  } catch (error) {
    if (!(error instanceof PreprocessError)) {
      context.errors.push(error)
    }
    return undefined
  }

  return {
    importGraph,
    programs,
  }
}