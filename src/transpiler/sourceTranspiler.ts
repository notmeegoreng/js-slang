import { generate } from "astring";
import type * as es from "estree";

import { UNKNOWN_LOCATION } from "../constants";
import assert from "../utils/assert";
import { isVariableDeclaration } from "../utils/ast/typeGuards";
import * as create from '../utils/astCreator'
import { getUniqueId } from "../utils/uniqueIds";
import { ProgramTranspiler } from "./baseTranspiler";

const globalIdNames = [
  'native',
  'callIfFuncAndRightArgs',
  'boolOrErr',
  'wrap',
  'wrapSourceModule',
  'unaryOp',
  'binaryOp',
  'throwIfTimeout',
  'setProp',
  'getProp',
  'builtins'
] as const

function getComputedProperty(computed: boolean, property: es.Expression): es.Expression {
  return computed ? property : create.literal((property as es.Identifier).name)
}

export default class SourceProgramTranspiler extends ProgramTranspiler {
  private createFunctionCall(name: typeof globalIdNames[number]) {
    const nativeExpr = create.memberExpression(
      this.localObjId,
      'native',
    )

    if (name === 'native') return nativeExpr

    return create.callExpression(
      create.memberExpression(
        create.memberExpression(
          nativeExpr,
        'operators'
        ),
        'get'
      ),
      [create.literal(name)]
    )
  }

  private transpileFunction(node: es.FunctionDeclaration | es.FunctionExpression | es.ArrowFunctionExpression) {
    const transformLogicalExpression = (expression: es.Expression): es.Expression => {
      switch (expression.type) {
        case 'LogicalExpression':
          return create.logicalExpression(
            expression.operator,
            this.transpileExpression(expression.left),
            transformLogicalExpression(expression.right),
            expression.loc
          )
        case 'ConditionalExpression':
          return create.conditionalExpression(
            this.transpileExpression(expression.test),
            transformLogicalExpression(expression.consequent),
            transformLogicalExpression(expression.alternate),
            expression.loc
          )
        case 'CallExpression':
          const { line, column } = (expression.loc ?? UNKNOWN_LOCATION).start
          const source = expression.loc?.source ?? null
          const functionName =
            expression.callee.type === 'Identifier' ? expression.callee.name : '<anonymous>'

          const args = expression.arguments

          return create.objectExpression([
            create.property('isTail', create.literal(true)),
            // TODO need to transpile callee??
            create.property('function', expression.callee as es.Expression),
            create.property('functionName', create.literal(functionName)),
            create.property('arguments', create.arrayExpression(args as es.Expression[])),
            create.property('line', create.literal(line)),
            create.property('column', create.literal(column)),
            create.property('source', create.literal(source))
          ])
        default:
          return create.objectExpression([
            create.property('isTail', create.literal(false)),
            create.property('value', this.transpileExpression(expression))
          ])
      }
    }   

    const body = node.body.type === 'BlockStatement' ? node.body : create.blockStatement([
      create.returnStatement(node.body)
    ])

    const newBody = body.body.map(stmt => {
      if (stmt.type === 'ReturnStatement') {
        return create.returnStatement(
          transformLogicalExpression(stmt.argument!)
        )
      }
      return stmt
    })

    return create.callExpression(
      this.createFunctionCall('wrap'),
      [
        create.blockArrowFunction(
          node.params as es.Identifier[],
          create.blockStatement(newBody),
        ),
        create.literal(generate(node)),
        create.literal(node.params[node.params.length - 1]?.type === 'RestElement'),
        this.createFunctionCall('native')
      ]
    )
  }

  private getTimeAst = () => create.callExpression(create.identifier('get_time'), [])

  private transpileLoopBody(timeId: string, node: es.WhileStatement | es.ForStatement) {
    const { line, column } = (node.loc ?? UNKNOWN_LOCATION).start
    const source = node.loc?.source ?? null

    const newBody = node.body.type === 'BlockStatement'
      ? this.transpileStatement(node.body)
      : create.blockStatement([this.transpileStatement(node.body)])

    newBody.body.unshift(
      create.expressionStatement(
        create.callExpression(
          this.createFunctionCall('throwIfTimeout'),
          [
            this.createFunctionCall('native'),
            create.identifier(timeId),
            this.getTimeAst(),
            create.literal(line),
            create.literal(column),
            create.literal(source)
          ]
        )
      )
    )

    return newBody
  }

  protected override transpileExpression(node: es.Expression): es.Expression {
    const { line, column } = (node.loc ?? UNKNOWN_LOCATION).start
    const source = node.loc?.source ?? null

    switch (node.type) {
      case 'ArrowFunctionExpression':
      case 'FunctionExpression':
        return this.transpileFunction(node)
      case 'AssignmentExpression': {
        if (node.left.type === 'MemberExpression') {
          const { object, property, computed, loc } = node.left
          const { line, column } = (loc ?? UNKNOWN_LOCATION).start
          const source = loc?.source ?? null
          return create.callExpression(
            this.createFunctionCall('setProp'),
            [
              this.transpileExpression(object as es.Expression),
              getComputedProperty(computed, property as es.Expression),
              this.transpileExpression(node.right),
              create.literal(line),
              create.literal(column),
              create.literal(source)
            ]
          )
        }
        return node
      }
      case 'BinaryExpression':
        return create.callExpression(
          this.createFunctionCall('binaryOp'),
          [
            create.literal(node.operator),
            create.literal(this.context.chapter),
            this.transpileExpression(node.left),
            this.transpileExpression(node.right),
            create.literal(line),
            create.literal(column),
            create.literal(source)
          ]
        )
      case 'CallExpression': {
        const args = node.arguments.map(each => {
          return each.type === 'SpreadElement' ? each : this.transpileExpression(each);
        })

        return create.callExpression(
          this.createFunctionCall('callIfFuncAndRightArgs'),
          [
            node.callee as es.Expression,
            create.literal(line),
            create.literal(column),
            create.literal(source),
            ...args
          ]
        )
      }
      case 'ConditionalExpression':
        return create.conditionalExpression(
          create.callExpression(
            this.createFunctionCall('boolOrErr'), [
              this.transpileExpression(node.test),
              create.literal(line),
              create.literal(column),
              create.literal(source)
            ]
          ),
          node.consequent,
          node.alternate
        )
      case 'LogicalExpression':
        return create.conditionalExpression(
          create.callExpression(
            this.createFunctionCall('boolOrErr'), [
              this.transpileExpression(node.left),
              create.literal(line),
              create.literal(column),
              create.literal(source)
            ]
          ),
          this.transpileExpression(node.right),
          create.literal(false)
        )
      case 'MemberExpression':
        return create.callExpression(
          this.createFunctionCall('getProp'),
          [
            node.object as es.Expression,
            getComputedProperty(node.computed, node.property as es.Expression),
            create.literal(line),
            create.literal(column),
            create.literal(source)
          ]
        )
      case 'UnaryExpression':
        return create.callExpression(
          this.createFunctionCall('unaryOp'),
          [
            create.literal(node.operator),
            this.transpileExpression(node.argument),
            create.literal(line),
            create.literal(column),
            create.literal(source)
          ]
        )
      default:
        return node
    }
  }

  protected transpileStatement(node: es.BlockStatement): es.BlockStatement
  protected transpileStatement(node: es.Statement): es.Statement
  protected override transpileStatement(node: es.Statement): es.Statement {
    switch (node.type) {
      case 'BlockStatement':
        return create.blockStatement(node.body.map(this.transpileStatement))
      case 'ExpressionStatement':
        return create.expressionStatement(
          this.transpileExpression(node.expression)
        )
      case 'FunctionDeclaration':
        return create.constantDeclaration(
          node.id!.name,
          this.transpileFunction(node)
        )
      case 'ForStatement': {
        const timeDecl = getUniqueId(this.localIdentifiers, 'startTime')
        const newLoopBody = this.transpileLoopBody(timeDecl, node)

        const newInit = isVariableDeclaration(node.init!)
          ? this.transpileStatement(node.init) as es.VariableDeclaration
          : this.transpileExpression(node.init!)

        return create.blockStatement([
          create.constantDeclaration(
            timeDecl,
            this.getTimeAst()
          ),
          create.forStatement(
            newInit,
            this.transpileExpression(node.test!),
            this.transpileExpression(node.update!),
            newLoopBody
          )
        ])
      }
      case 'IfStatement':
        return create.ifStatement(
          this.transpileExpression(node.test),
          this.transpileStatement(node.consequent),
          this.transpileStatement(node.alternate!),
        )
      case 'VariableDeclaration': {
        assert(node.declarations.length === 1, 'VariableDeclarations in Source should only have 1 declarator')
        const { declarations: [decl] } = node
        assert(decl.id.type === 'Identifier', 'VariableDeclarations in Source should only use identifiers')

        return create.variableDeclaration([
          create.variableDeclarator(
            decl.id,
            this.transpileExpression(decl.init!)
          ),
          ], 
          node.kind
        )
      }
      case 'WhileStatement': {
        const timeDecl = getUniqueId(this.localIdentifiers, 'startTime')
        const newLoopBody = this.transpileLoopBody(timeDecl, node)

        return create.blockStatement([
          create.constantDeclaration(
            timeDecl,
            this.getTimeAst()
          ),
          create.whileStatement(
            newLoopBody,
            this.transpileExpression(node.test),
            )
          ])
        }
      default:
        return node
      }
  }
}
