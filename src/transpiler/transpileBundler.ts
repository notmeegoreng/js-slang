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
import {
  getIdentifiersInNativeStorage,
  getIdentifiersInProgram,
  getUniqueId
} from '../utils/uniqueIds'
import { NATIVE_STORAGE_ID } from '../constants'
import type { LinkerSuccess } from '../modules/preprocessor/linker'
import { parse } from '../parser/parser'
import assert from '../utils/assert'
import type { Bundler } from '../modules/preprocessor/bundler'
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

function getBuiltinsAndPrelude(
  globalNativeId: es.Identifier,
  evaluatePrelude: boolean,
  context: Context
): [es.Program['body'], es.Program | null] {
  const newStatements: es.VariableDeclaration[] = []
  // Only if we haven't evaluated any code before
  // do we try to evaluate builtins and the prelude
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

    if (evaluatePrelude && context.prelude !== null) {
      const prelude = parse(context.prelude, context)
      assert(prelude !== null, 'Prelude should not have parser errors')
      return [newStatements, prelude]
    }
  }
  return [newStatements, null]
}

function transpileFiles(
  { programs, entrypointFilePath, topoOrder }: Pick<LinkerSuccess, 'programs' | 'entrypointFilePath' | 'topoOrder'>,
  context: Context,
  evaluatePrelude: boolean,
  fileTranspiler: FileTranspiler
): es.Program | undefined {
  try {
    const entrypointProgram = programs[entrypointFilePath]

    const globalIdentifiers = new Set([
      ...getIdentifiersInNativeStorage(context.nativeStorage),
      ...getIdentifiersInProgram(entrypointProgram)
    ])

    const globalNativeId = ast.identifier(getUniqueId(globalIdentifiers, NATIVE_STORAGE_ID))

    function transpileSingleFile(
      fileName: string,
      nativeId: es.Identifier | null,
      isEntrypoint: boolean
    ) {
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
        false
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

    const [builtins, prelude] = getBuiltinsAndPrelude(globalNativeId, evaluatePrelude, context)
    const transpiledPrograms = topoOrder
      .filter(fileName => fileName !== entrypointFilePath)
      .map(fileName => transpileSingleFile(fileName, null, false))
    const entrypointTranspiled = transpileSingleFile(entrypointFilePath, globalNativeId, true)

    getIdentifiersDeclaredByProgram(entrypointProgram).forEach(name =>
      context.nativeStorage.previousProgramsIdentifiers.add(name)
    )

    if (prelude) {
      getIdentifiersDeclaredByProgram(prelude).forEach(name =>
        context.nativeStorage.previousProgramsIdentifiers.add(name)
      )
      prelude.body.forEach(stmt => builtins.push(stmt))
    }

    return ast.program([...builtins, ...transpiledPrograms, entrypointTranspiled])
  } catch (error) {
    context.errors.push(error)
    return undefined
  }
}

export const getNativeTranspiler =
  (fileTranspiler: FileTranspiler, evaluatePrelude: boolean): Bundler =>
  (linker, context) => transpileFiles(linker, context, evaluatePrelude, fileTranspiler)

export const transpileFilesToSource = getNativeTranspiler(sourceFileTranspiler, true)
export const transpileFilesToFullJS = getNativeTranspiler(prog => [[], prog.body], false)
