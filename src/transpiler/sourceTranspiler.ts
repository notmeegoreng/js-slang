import * as _ from 'lodash'
import type es from 'estree'
import { generate } from 'astring'
import * as ast from '../utils/ast/astCreator'
import type { Context, Node, Chapter } from '../types'
import { UNKNOWN_LOCATION } from '../constants'
import { simple } from '../utils/walkers'
import { getIdentifiersInNativeStorage, getUniqueId } from '../utils/uniqueIds'
import { getIdentifiersDeclaredByProgram } from '../utils/ast/helpers'

type Program2 = (es.Statement | es.Declaration)[]
export type FileTranspiler = (
  program: es.Program,
  context: Context,
  nativeId: es.Identifier,
  isEntrypoint: boolean
) => [Program2, es.Program['body']]

const transpilerInternalNames = [
  'callIfFuncAndRightArgs',
  'boolOrErr',
  'wrap',
  'unaryOp',
  'binaryOp',
  'throwIfTimeout',
  'setProp',
  'getProp',
  'builtins',
  'native'
] as const

export type TranspilerInternalsIds = Record<(typeof transpilerInternalNames)[number], es.Identifier>
function getTranspilerInternalIds(
  nativeId: es.Identifier,
  usedIdentifiers: Set<string>
): TranspilerInternalsIds {
  return transpilerInternalNames.reduce(
    (res, identifier) =>
      identifier === 'native'
        ? res
        : {
            ...res,
            [identifier]: ast.identifier(getUniqueId(usedIdentifiers, identifier))
          },
    {
      native: nativeId
    } as TranspilerInternalsIds
  )
}

function generateFunctionsToStringMap(program: es.Program) {
  const map: Map<Node, string> = new Map()
  simple(program, {
    ArrowFunctionExpression(node: es.ArrowFunctionExpression) {
      map.set(node, generate(node))
    },
    FunctionDeclaration(node: es.FunctionDeclaration) {
      map.set(node, generate(node))
    }
  })
  return map
}

function transformFunctionDeclarationsToArrowFunctions(
  program: es.Program,
  functionsToStringMap: Map<Node, string>
) {
  simple(program, {
    FunctionDeclaration(node) {
      const { id, params, body } = node as es.FunctionDeclaration
      node.type = 'VariableDeclaration'
      node = node as es.VariableDeclaration
      const asArrowFunction = ast.blockArrowFunction(params as es.Identifier[], body)
      functionsToStringMap.set(asArrowFunction, functionsToStringMap.get(node)!)
      node.declarations = [
        {
          type: 'VariableDeclarator',
          id: id as es.Identifier,
          init: asArrowFunction
        }
      ]
      node.kind = 'const'
    }
  })
}

/**
 * Transforms all arrow functions
 * (arg1, arg2, ...) => { statement1; statement2; return statement3; }
 *
 * to
 *
 * <NATIVE STORAGE>.operators.wrap((arg1, arg2, ...) => {
 *   statement1;statement2;return statement3;
 * })
 *
 * to allow for iterative processes to take place
 */

function wrapArrowFunctionsToAllowNormalCallsAndNiceToString(
  program: es.Program,
  functionsToStringMap: Map<Node, string>,
  globalIds: TranspilerInternalsIds
) {
  simple(program, {
    ArrowFunctionExpression(node: es.ArrowFunctionExpression) {
      // If it's undefined then we're dealing with a thunk
      if (functionsToStringMap.get(node)! !== undefined) {
        ast.mutateToCallExpression(node, globalIds.wrap, [
          { ...node },
          ast.literal(functionsToStringMap.get(node)!),
          ast.literal(node.params[node.params.length - 1]?.type === 'RestElement'),

          globalIds.native
        ])
      }
    }
  })
}

/**
 * Transforms all return statements (including expression arrow functions) to return an intermediate value
 * return nonFnCall + 1;
 *  =>
 * return {isTail: false, value: nonFnCall + 1};
 *
 * return fnCall(arg1, arg2);
 * => return {isTail: true, function: fnCall, arguments: [arg1, arg2]}
 *
 * conditional and logical expressions will be recursively looped through as well
 */
function transformReturnStatementsToAllowProperTailCalls(program: es.Program) {
  function transformLogicalExpression(expression: es.Expression): es.Expression {
    switch (expression.type) {
      case 'LogicalExpression':
        return ast.logicalExpression(
          expression.operator,
          expression.left,
          transformLogicalExpression(expression.right),
          expression.loc
        )
      case 'ConditionalExpression':
        return ast.conditionalExpression(
          expression.test,
          transformLogicalExpression(expression.consequent),
          transformLogicalExpression(expression.alternate),
          expression.loc
        )
      case 'CallExpression':
        expression = expression as es.CallExpression
        const { line, column } = (expression.loc ?? UNKNOWN_LOCATION).start
        const source = expression.loc?.source ?? null
        const functionName =
          expression.callee.type === 'Identifier' ? expression.callee.name : '<anonymous>'

        const args = expression.arguments

        return ast.objectExpression([
          ast.property('isTail', ast.literal(true)),
          ast.property('function', expression.callee as es.Expression),
          ast.property('functionName', ast.literal(functionName)),
          ast.property('arguments', ast.arrayExpression(args as es.Expression[])),
          ast.property('line', ast.literal(line)),
          ast.property('column', ast.literal(column)),
          ast.property('source', ast.literal(source))
        ])
      default:
        return ast.objectExpression([
          ast.property('isTail', ast.literal(false)),
          ast.property('value', expression)
        ])
    }
  }

  simple(program, {
    ReturnStatement(node: es.ReturnStatement) {
      node.argument = transformLogicalExpression(node.argument!)
    },
    ArrowFunctionExpression(node: es.ArrowFunctionExpression) {
      if (node.expression) {
        node.body = transformLogicalExpression(node.body as es.Expression)
      }
    }
  })
}

function transformCallExpressionsToCheckIfFunction(
  program: es.Program,
  globalIds: TranspilerInternalsIds
) {
  simple(program, {
    CallExpression(node: es.CallExpression) {
      const { line, column } = (node.loc ?? UNKNOWN_LOCATION).start
      const source = node.loc?.source ?? null
      const args = node.arguments

      node.arguments = [
        node.callee as es.Expression,
        ast.literal(line),
        ast.literal(column),
        ast.literal(source),
        ...args
      ]

      node.callee = globalIds.callIfFuncAndRightArgs
    }
  })
}

function transformSomeExpressionsToCheckIfBoolean(
  program: es.Program,
  globalIds: TranspilerInternalsIds
) {
  function transform(
    node:
      | es.IfStatement
      | es.ConditionalExpression
      | es.LogicalExpression
      | es.ForStatement
      | es.WhileStatement
  ) {
    const { line, column } = (node.loc ?? UNKNOWN_LOCATION).start
    const source = node.loc?.source ?? null
    const test = node.type === 'LogicalExpression' ? 'left' : 'test'
    node[test] = ast.callExpression(globalIds.boolOrErr, [
      node[test],
      ast.literal(line),
      ast.literal(column),
      ast.literal(source)
    ])
  }

  simple(program, {
    IfStatement: transform,
    ConditionalExpression: transform,
    LogicalExpression: transform,
    ForStatement: transform,
    WhileStatement: transform
  })
}

function transformUnaryAndBinaryOperationsToFunctionCalls(
  program: es.Program,
  globalIds: TranspilerInternalsIds,
  chapter: Chapter
) {
  simple(program, {
    BinaryExpression(node: es.BinaryExpression) {
      const { line, column } = (node.loc ?? UNKNOWN_LOCATION).start
      const source = node.loc?.source ?? null
      const { operator, left, right } = node
      ast.mutateToCallExpression(node, globalIds.binaryOp, [
        ast.literal(operator),
        ast.literal(chapter),
        left,
        right,
        ast.literal(line),
        ast.literal(column),
        ast.literal(source)
      ])
    },
    UnaryExpression(node: es.UnaryExpression) {
      const { line, column } = (node.loc ?? UNKNOWN_LOCATION).start
      const source = node.loc?.source ?? null
      const { operator, argument } = node as es.UnaryExpression
      ast.mutateToCallExpression(node, globalIds.unaryOp, [
        ast.literal(operator),
        argument,
        ast.literal(line),
        ast.literal(column),
        ast.literal(source)
      ])
    }
  })
}

function getComputedProperty(computed: boolean, property: es.Expression): es.Expression {
  return computed ? property : ast.literal((property as es.Identifier).name)
}

function transformPropertyAssignment(program: es.Program, globalIds: TranspilerInternalsIds) {
  simple(program, {
    AssignmentExpression(node: es.AssignmentExpression) {
      if (node.left.type === 'MemberExpression') {
        const { object, property, computed, loc } = node.left
        const { line, column } = (loc ?? UNKNOWN_LOCATION).start
        const source = loc?.source ?? null
        ast.mutateToCallExpression(node, globalIds.setProp, [
          object as es.Expression,
          getComputedProperty(computed, property as es.Expression),
          node.right,
          ast.literal(line),
          ast.literal(column),
          ast.literal(source)
        ])
      }
    }
  })
}

function transformPropertyAccess(program: es.Program, globalIds: TranspilerInternalsIds) {
  simple(program, {
    MemberExpression(node: es.MemberExpression) {
      const { object, property, computed, loc } = node
      const { line, column } = (loc ?? UNKNOWN_LOCATION).start
      const source = loc?.source ?? null
      ast.mutateToCallExpression(node, globalIds.getProp, [
        object as es.Expression,
        getComputedProperty(computed, property as es.Expression),
        ast.literal(line),
        ast.literal(column),
        ast.literal(source)
      ])
    }
  })
}

function addInfiniteLoopProtection(
  program: es.Program,
  globalIds: TranspilerInternalsIds,
  usedIdentifiers: Set<string>
) {
  const getTimeAst = () => ast.callExpression(ast.identifier('get_time'), [])

  function instrumentLoops(node: es.Program | es.BlockStatement) {
    const newStatements = []
    for (const statement of node.body) {
      if (statement.type === 'ForStatement' || statement.type === 'WhileStatement') {
        const startTimeConst = getUniqueId(usedIdentifiers, 'startTime')
        newStatements.push(ast.constantDeclaration(startTimeConst, getTimeAst()))
        if (statement.body.type === 'BlockStatement') {
          const { line, column } = (statement.loc ?? UNKNOWN_LOCATION).start
          const source = statement.loc?.source ?? null
          statement.body.body.unshift(
            ast.expressionStatement(
              ast.callExpression(globalIds.throwIfTimeout, [
                globalIds.native,
                ast.identifier(startTimeConst),
                getTimeAst(),
                ast.literal(line),
                ast.literal(column),
                ast.literal(source)
              ])
            )
          )
        }
      }
      newStatements.push(statement)
    }
    node.body = newStatements
  }

  simple(program, {
    Program: instrumentLoops,
    BlockStatement: instrumentLoops
  })
}

function getDeclarationsToAccessTranspilerInternals(
  globalIds: TranspilerInternalsIds
): es.VariableDeclaration[] {
  return Object.entries(globalIds).reduce((res, [key, { name }]) => {
    switch (key) {
      case 'native':
        return res
      case 'globals':
        return [
          ...res,
          ast.constantDeclaration(name, ast.memberExpression(globalIds.native, 'globals'))
        ]
      default:
        return [
          ...res,
          ast.constantDeclaration(
            name,
            ast.callExpression(
              ast.memberExpression(ast.memberExpression(globalIds.native, 'operators'), 'get'),
              [ast.literal(key)]
            )
          )
        ]
    }
  }, [] as es.VariableDeclaration[])
}

export const transpileFileToSource: FileTranspiler = (rawProgram, context, nativeId) => {
  const program = _.cloneDeep(rawProgram)
  const usedIdentifiers = new Set([
    ...getIdentifiersInNativeStorage(context.nativeStorage),
    ...getIdentifiersDeclaredByProgram(program)
  ])

  const transpilerInternalIds = getTranspilerInternalIds(nativeId, usedIdentifiers)

  const functionsToStringMap = generateFunctionsToStringMap(program)

  transformReturnStatementsToAllowProperTailCalls(program)
  transformCallExpressionsToCheckIfFunction(program, transpilerInternalIds)
  transformUnaryAndBinaryOperationsToFunctionCalls(program, transpilerInternalIds, context.chapter)
  transformSomeExpressionsToCheckIfBoolean(program, transpilerInternalIds)
  transformPropertyAssignment(program, transpilerInternalIds)
  transformPropertyAccess(program, transpilerInternalIds)
  transformFunctionDeclarationsToArrowFunctions(program, functionsToStringMap)
  wrapArrowFunctionsToAllowNormalCallsAndNiceToString(
    program,
    functionsToStringMap,
    transpilerInternalIds
  )
  addInfiniteLoopProtection(program, transpilerInternalIds, usedIdentifiers)
  return [getDeclarationsToAccessTranspilerInternals(transpilerInternalIds), program.body]
}
