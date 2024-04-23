import * as _ from 'lodash'
import type es from 'estree'

import * as create from '../utils/ast/astCreator'
import { getIdentifiersInProgram } from '../utils/uniqueIds'
import { simple } from '../utils/walkers'
import { transpileFileToSource, type FileTranspiler } from '../transpiler/sourceTranspiler'
import { getNativeTranspiler, transpileFilesToSource } from '../transpiler/transpileBundler'
import type { Bundler } from '../modules/preprocessor/bundler'

const lazyPrimitives = new Set(['makeLazyFunction', 'wrapLazyCallee', 'forceIt', 'delayIt'])

const forcingNodes = new Set(['BinaryExpression', 'UnaryExpression'])

function transformFunctionDeclarationsToArrowFunctions(program: es.Program) {
  simple(program, {
    FunctionDeclaration(node) {
      const { id, params, body } = node as es.FunctionDeclaration
      if (id === null) {
        throw new Error(
          'Encountered a FunctionDeclaration node without an identifier. This should have been caught when parsing.'
        )
      }
      node.type = 'VariableDeclaration'
      node = node as es.VariableDeclaration
      const asArrowFunction = create.callExpression(
        create.identifier('makeLazyFunction', node.loc),
        [create.blockArrowFunction(params as es.Identifier[], body, node.loc)],
        node.loc
      )
      node.declarations = [
        {
          type: 'VariableDeclarator',
          id,
          init: asArrowFunction
        }
      ]
      node.kind = 'const'
    }
  })
}

function insertDelayAndForce(program: es.Program) {
  function transformConditionals(
    node:
      | es.IfStatement
      | es.ConditionalExpression
      | es.LogicalExpression
      | es.ForStatement
      | es.WhileStatement
  ) {
    const test = node.type === 'LogicalExpression' ? 'left' : 'test'
    if (forcingNodes.has(node[test].type)) {
      return
    }
    node[test] = create.callExpression(create.identifier('forceIt'), [node[test]], node.loc)
  }

  simple(program, {
    BinaryExpression(node: es.BinaryExpression) {
      node.left = create.callExpression(
        create.identifier('forceIt'),
        [node.left as es.Expression],
        node.left.loc
      )
      node.right = create.callExpression(
        create.identifier('forceIt'),
        [node.right as es.Expression],
        node.right.loc
      )
    },
    UnaryExpression(node: es.UnaryExpression) {
      node.argument = create.callExpression(
        create.identifier('forceIt'),
        [node.argument as es.Expression],
        node.argument.loc
      )
    },
    IfStatement: transformConditionals,
    ConditionalExpression: transformConditionals,
    LogicalExpression: transformConditionals,
    ForStatement: transformConditionals,
    WhileStatement: transformConditionals,
    CallExpression(node: es.CallExpression) {
      if (node.callee.type === 'Identifier' && lazyPrimitives.has(node.callee.name)) {
        return
      }
      node.callee = create.callExpression(
        create.identifier('wrapLazyCallee', node.callee.loc),
        [node.callee as es.Expression],
        node.callee.loc
      )
      node.arguments = node.arguments.map(arg =>
        create.callExpression(
          create.identifier('delayIt'),
          [create.arrowFunctionExpression([], arg as es.Expression, arg.loc)],
          arg.loc
        )
      )
    }
  })
}

const lazyFileTranspiler: FileTranspiler = (rawProgram, context, nativeId, isEntrypoint) => {
  const program = _.cloneDeep(rawProgram)
  transformFunctionDeclarationsToArrowFunctions(program)
  insertDelayAndForce(program)
  return transpileFileToSource(program, context, nativeId, isEntrypoint)
}

const lazyFilesTranspiler = getNativeTranspiler(lazyFileTranspiler)

export const transpileFilesToLazy: Bundler = (linkerSuccess, context) => {
  const isManual = Object.values(linkerSuccess.programs).find(program => {
    const identifiers = getIdentifiersInProgram(program)
    return identifiers.has('forceIt') || identifiers.has('delayIt')
  })

  if (isManual) {
    const outputProgram = transpileFilesToSource(linkerSuccess, context)
    if (!outputProgram) return undefined

    outputProgram.body.unshift(
      create.expressionStatement(
        create.callExpression(
          create.identifier('display'),
          [
            create.literal(
              'Manual use of lazy library detected, turning off automatic lazy evaluation transformation.'
            )
          ],
          {
            start: { line: 0, column: 0 },
            end: { line: 0, column: 0 }
          }
        )
      )
    )
  }

  return lazyFilesTranspiler(linkerSuccess, context)
}

// transpiles if possible and modifies program to a Source program that makes use of lazy primitives
export function transpileToLazy(program: es.Program) {
  const identifiers = getIdentifiersInProgram(program)
  if (identifiers.has('forceIt') || identifiers.has('delayIt')) {
    program.body.unshift(
      create.expressionStatement(
        create.callExpression(
          create.identifier('display'),
          [
            create.literal(
              'Manual use of lazy library detected, turning off automatic lazy evaluation transformation.'
            )
          ],
          {
            start: { line: 0, column: 0 },
            end: { line: 0, column: 0 }
          }
        )
      )
    )
    return
  }

  transformFunctionDeclarationsToArrowFunctions(program)
  insertDelayAndForce(program)
}
