import type es from 'estree'
import type { Context } from '..'
import { isModuleDeclaration, isNamespaceSpecifier } from '../utils/ast/typeGuards'
import {
  getIdentifiersDeclaredByProgram,
  getIdsFromDeclaration,
  getImportedName,
  getModuleDeclarationSource
} from '../utils/ast/helpers'
import * as ast from '../utils/ast/astCreator'
import { getIdentifiersInNativeStorage, getUniqueId } from '../utils/uniqueIds'
import { NATIVE_STORAGE_ID } from '../constants'
import type { LinkerSuccess } from '../modules/preprocessor/linker'
import { checkForUndefinedVariables } from '../validator/validator'
import { evallerReplacer } from './transpiler'
import {
  transpileFileToSource as sourceFileTranspiler,
  type FileTranspiler
} from './sourceTranspiler'

type ExportEntry = [string, es.Expression] | es.SpreadElement
type Program2 = (es.Statement | es.Declaration)[]

function processModuleDeclarations(
  statements: es.Program['body'],
  nativeId: es.Identifier
): [Program2, ExportEntry[]] {
  const newEntry = (name: string, expr: es.Expression): ExportEntry => [name, expr]

  return statements.reduce(
    ([newNodes, pairs], node) => {
      if (!isModuleDeclaration(node)) {
        if (node.type === 'TryStatement') {
          // To make this work with the infinite loop detector
          const [newTryBlock, blockExports] = processModuleDeclarations(node.block.body, nativeId)
          const [newFinalizerBlock, finalizerExports] = node.finalizer
            ? processModuleDeclarations(node.finalizer.body, nativeId)
            : [[], []]

          const newTry: es.TryStatement = {
            ...node,
            block: ast.blockStatement(newTryBlock),
            finalizer: ast.blockStatement(newFinalizerBlock)
          }

          return [
            [...newNodes, newTry],
            [...pairs, ...blockExports, ...finalizerExports]
          ]
        }

        return [[...newNodes, node], pairs]
      }

      if (node.type === 'ExportDefaultDeclaration') {
        switch (node.declaration.type) {
          case 'ClassDeclaration':
          case 'FunctionDeclaration': {
            if (node.declaration.id) {
              return [
                [...newNodes, node.declaration],
                [...pairs, newEntry('default', node.declaration.id)]
              ]
            }
          }
        }

        return [
          newNodes,
          [...pairs, newEntry('default', node.declaration as unknown as es.Expression)]
        ]
      }

      if (node.type === 'ExportNamedDeclaration') {
        if (node.declaration) {
          const ids = getIdsFromDeclaration(node.declaration)
          return [
            [...newNodes, node.declaration],
            [...pairs, ...ids.map(id => newEntry(id.name, id))]
          ]
        }

        if (!node.source) {
          return [
            newNodes,
            [...pairs, ...node.specifiers.map(spec => newEntry(spec.exported.name, spec.local))]
          ]
        }
      }

      const source = getModuleDeclarationSource(node)
      const modulesObj = ast.computedMemberExpression(
        ast.memberExpression(nativeId, 'loadedModules'),
        source
      )

      switch (node.type) {
        case 'ExportAllDeclaration':
          return [
            newNodes,
            [
              ...pairs,
              node.exported
                ? newEntry(node.exported.name, modulesObj)
                : ({ type: 'SpreadElement', argument: modulesObj } as ExportEntry)
            ]
          ]
        case 'ExportNamedDeclaration':
          return [
            newNodes,
            [
              ...pairs,
              ...node.specifiers.map(spec =>
                newEntry(spec.exported.name, ast.memberExpression(modulesObj, spec.local.name))
              )
            ]
          ]
      }

      const declNodes = node.specifiers.map(spec =>
        ast.constantDeclaration(
          spec.local.name,
          isNamespaceSpecifier(spec)
            ? modulesObj
            : ast.memberExpression(modulesObj, getImportedName(spec))
        )
      )
      return [[...newNodes, ...declNodes], pairs]
    },
    [[], []] as [Program2, ExportEntry[]]
  )
}

function getBuiltins(globalNativeId: es.Identifier, context: Context): es.Program['body'] {
  const newStatements: es.VariableDeclaration[] = []
  // Only if we haven't evaluated any code before
  // do we try to evaluate builtins
  if (context.nativeStorage.evaller === null) {
    for (const builtin of context.nativeStorage.builtins.keys()) {
      newStatements.push(
        ast.constantDeclaration(
          builtin,
          ast.callExpression(
            ast.memberExpression(ast.memberExpression(globalNativeId, 'builtins'), 'get'),
            [ast.literal(builtin)]
          )
        )
      )
    }
  }

  // Just in case anyone is wondering why we don't just evaluate the user program and
  // prelude at the same time:
  // If we do, the line numbers for the errors that get become wrong, because then the
  // program starts at the prelude, not with the user's program
  return newStatements
}

function transpileFiles(
  {
    programs,
    entrypointFilePath,
    topoOrder
  }: Pick<LinkerSuccess, 'programs' | 'entrypointFilePath' | 'topoOrder'>,
  context: Context,
  fileTranspiler: FileTranspiler,
  skipUndefined: boolean
): es.Program | undefined {
  try {
    const entrypointProgram = programs[entrypointFilePath]

    // const globalIdentifiers = new Set([
    //   ...getIdentifiersInNativeStorage(context.nativeStorage),
    //   ...getIdentifiersInProgram(entrypointProgram)
    // ])

    const globalNativeId = ast.identifier(NATIVE_STORAGE_ID)

    function transpileSingleFile(
      fileName: string,
      nativeId: es.Identifier | null,
      isEntrypoint: boolean
    ) {
      checkForUndefinedVariables(programs[fileName], context, {} as any, skipUndefined)

      const usedIdentifiers = new Set([
        ...getIdentifiersInNativeStorage(context.nativeStorage),
        ...getIdentifiersDeclaredByProgram(programs[fileName])
      ])

      const localNativeId =
        nativeId ?? ast.identifier(getUniqueId(usedIdentifiers, NATIVE_STORAGE_ID))

      const [internalsCode, userCode] = fileTranspiler(
        programs[fileName],
        context,
        localNativeId,
        isEntrypoint
      )

      const [newBody, exportEntries] = processModuleDeclarations(userCode, localNativeId)

      if (!isEntrypoint) {
        const exportObj = ast.objectExpression(
          exportEntries.map(entry => {
            if (Array.isArray(entry)) return ast.property(entry[0], entry[1])
            return entry
          })
        )

        const exportAssignmentStmt = ast.expressionStatement(
          ast.assignmentExpression(
            ast.computedMemberExpression(
              ast.memberExpression(localNativeId, 'loadedModules'),
              fileName
            ),
            exportObj
          )
        )

        return ast.expressionStatement(
          ast.callExpression(
            ast.arrowFunctionExpression(
              [localNativeId],
              ast.blockStatement([...internalsCode, ...newBody, exportAssignmentStmt])
            ),
            [globalNativeId]
          )
        )
      }

      return ast.blockStatement([
        ...internalsCode,
        evallerReplacer(globalNativeId, usedIdentifiers),
        ast.expressionStatement(ast.identifier('undefined')),
        ...newBody
      ])
    }

    const builtins = getBuiltins(globalNativeId, context)
    const transpiledPrograms = topoOrder
      .filter(fileName => fileName !== entrypointFilePath)
      .map(fileName => transpileSingleFile(fileName, null, false))
    const entrypointTranspiled = transpileSingleFile(entrypointFilePath, globalNativeId, true)

    getIdentifiersDeclaredByProgram(entrypointProgram).forEach(name =>
      context.nativeStorage.previousProgramsIdentifiers.add(name)
    )

    // if (prelude) {
    //   getIdentifiersDeclaredByProgram(prelude).forEach(name =>
    //     context.nativeStorage.previousProgramsIdentifiers.add(name)
    //   )
    //   prelude.body.forEach(stmt => builtins.push(stmt))
    // }

    return ast.program([...builtins, ...transpiledPrograms, entrypointTranspiled])
  } catch (error) {
    context.errors.push(error)
    return undefined
  }
}

export type NativeBundler = (
  res: Pick<LinkerSuccess, 'entrypointFilePath' | 'topoOrder' | 'programs'>,
  context: Context,
  skipUndefined?: boolean
) => es.Program | undefined

export function getNativeTranspiler(fileTranspiler: FileTranspiler, mainSkipUndefined?: boolean) {
  const filesTranspiler: NativeBundler = (linker, context, skipUndefined: boolean = false) =>
    transpileFiles(linker, context, fileTranspiler, mainSkipUndefined ?? skipUndefined)
  return filesTranspiler
}

export const transpileFilesToSource = getNativeTranspiler(sourceFileTranspiler)
export const transpileFilesToFullJS = getNativeTranspiler(prog => [[], prog.body], true)
