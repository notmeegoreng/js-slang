import type * as es from "estree";

import { Context } from "..";
import assert from "../utils/assert";
import { extractIdsFromPattern, getTopLevelIdentifiersInProgram } from "../utils/ast/helpers";
import { isModuleDeclaration, isSourceModule } from "../utils/ast/typeGuards";
import * as create from '../utils/astCreator'
import { getUniqueId } from "../utils/uniqueIds";

const INIT_OBJ_ID = '__OBJ__'

type BlockArrowFunction = Omit<es.ArrowFunctionExpression, 'expression' | 'body'> & {
  expression: false
  body: es.BlockStatement
}

export class ProgramTranspiler {
  public readonly localObjId: es.Identifier
  public readonly localIdentifiers: Set<string>

  constructor(
    public readonly program: es.Program,
    public readonly context: Context,
    objId: es.Identifier | null,
    usedIdentifiers: Set<string>
  ) {
    this.localIdentifiers = new Set([
      ...getTopLevelIdentifiersInProgram(program).map(({ name }) => name),
      ...usedIdentifiers,
    ])
    this.localObjId = objId ?? create.identifier(getUniqueId(this.localIdentifiers, INIT_OBJ_ID))
  }

  protected createModuleAccessExpr(moduleName: string, property?: string): es.Expression {
    const accessExpr = create.memberExpression(
      create.memberExpression(
        this.localObjId,
        'modules'
      ),
      moduleName,
      true
    );

    return property ? create.memberExpression(accessExpr, property) : accessExpr
  }

  protected transpileStatement(node: es.Statement): es.Statement { return node }
  protected transpileExpression(node: es.Expression): es.Expression { return node }

  protected createExportStatement(exportExpressions: ([string, es.Expression] | es.SpreadElement)[]) {
    return create.returnStatement(create.objectExpression(exportExpressions.map(entry => {
      if (Array.isArray(entry)) {
        const [name, expr] = entry
        return create.property(name, expr)
      }
      return entry
    })))
  }

  public transpile(): [BlockArrowFunction, Set<string>] {
    const exportExpressions: ([string, es.Expression] | es.SpreadElement)[] = []
    const sourceModulesToImport = new Set<string>();
    const newBody = this.program.body.reduce((res, node) => {
      if (node.type === 'ExportDefaultDeclaration') {
        switch (node.declaration.type) {
          case 'VariableDeclaration':
            throw new Error('ExportDefaultDeclarations should not have VariableDeclarations')
          case 'ClassDeclaration':
          case 'FunctionDeclaration': {
            if (node.declaration.id) {
              exportExpressions.push([
                'default',
                node.declaration.id,
              ])
              return [
                ...res,
                this.transpileStatement(node.declaration)
              ]
            }

            // Function or class declarations without identifiers
            // should be treated as their respective expression type
            if (node.declaration.type === 'FunctionDeclaration') {
              node.declaration = {
                ...node.declaration,
                type: 'FunctionExpression'
              }
            } else {
              node.declaration = {
                ...node.declaration,
                type: 'ClassExpression'
              }
            }
            // Case falls through!
          }
          default: {
            exportExpressions.push([
              'default',
              this.transpileExpression(node.declaration)
            ])
            return res
          }
        }
      } else if (node.type === 'ExportNamedDeclaration') {
        if (node.declaration) {
          switch (node.declaration.type) {
            case 'ClassDeclaration':
            case 'FunctionDeclaration': {
              assert(!!node.declaration.id, `Encountered a ${node.declaration.type} without an identifier, this should have been caught during parsing!`)
              exportExpressions.push([
                node.declaration.id.name,
                node.declaration.id
              ])
              return [
                this.transpileStatement(node.declaration),
                ...res,
              ]
            }
            case 'VariableDeclaration': {
              for (const { id } of node.declaration.declarations) {
                for (const each of extractIdsFromPattern(id)) {
                  exportExpressions.push([
                    each.name,
                    each
                  ])
                }
              }
              return [
                ...res,
                this.transpileStatement(node.declaration),
              ]
            }
          }
        } else if (!node.source) {
          node.specifiers.forEach(spec => {
            exportExpressions.push([
              spec.exported.name,
              spec.local,
            ])
          })
          return res
        }
      } else if (!isModuleDeclaration(node)) {
        return [
          ...res,
          // TODO transpile
          node
        ]
      }

      const source = node.source?.value
      assert(typeof source === 'string', `Expected import source value to be string, got ${source}`)
      const moduleAccessExpr = this.createModuleAccessExpr(source)

      if (isSourceModule(source)) {
        sourceModulesToImport.add(source)
      }

      switch (node.type) {
        case 'ExportAllDeclaration': {
          exportExpressions.push(node.exported ? [
            node.exported.name,
            moduleAccessExpr
          ] : {
            type: 'SpreadElement',
            argument: moduleAccessExpr
          })
          return res
        }
        case 'ExportNamedDeclaration': {
          node.specifiers.forEach(spec => {
            exportExpressions.push([
              spec.exported.name,
              create.memberExpression(
                moduleAccessExpr,
                spec.local.name,
              )
            ])
          })
          return res
        }
        case 'ImportDeclaration': {
          const newDecls = node.specifiers.map(spec => {
            switch (spec.type) {
              case 'ImportSpecifier':
                return create.constantDeclaration(
                  spec.local.name,
                  create.memberExpression(
                    moduleAccessExpr,
                    spec.imported.name
                  )
                )
              case 'ImportDefaultSpecifier':
                return create.constantDeclaration(
                  spec.local.name,
                  create.memberExpression(
                    moduleAccessExpr,
                    'default',
                  )
                )
              case 'ImportNamespaceSpecifier':
                return create.constantDeclaration(
                  spec.local.name,
                  moduleAccessExpr
                )
            }
          })
          return [
            ...newDecls,
            ...res,
          ]
        }
      }
    }, [] as (es.Declaration | es.Statement)[])

    const retStatement = this.createExportStatement(exportExpressions)

    return [{
      type: 'ArrowFunctionExpression',
      body: create.blockStatement([
        ...newBody,
        retStatement
      ]),
      expression: false,
      params: [this.localObjId]
    }, sourceModulesToImport]
  }
}

const getBuiltinDecls = (context: Context, objId: es.Identifier) => [...context.nativeStorage.builtins.keys()].map(builtin => create.constantDeclaration(
  builtin,
  create.callExpression(
    create.memberExpression(
      create.memberExpression(
        create.memberExpression(
          objId,
          'native'
        ),
        'builtins'
      ),
      'get'
    ), 
    [create.literal(builtin)]
  )
))

type ProgramTranspilerFactory = (...args: ConstructorParameters<typeof ProgramTranspiler>) => ProgramTranspiler

export default async function transpilePrograms(
  programs: Record<string, es.Program>,
  topoOrder: string[],
  context: Context,
  getTranspiler: ProgramTranspilerFactory,
  usedIdentifiers: Set<string>
) {
  // Entrypoint path should always be the last entry in the topological ordering
  const entrypointFilePath = topoOrder.pop()!
  const entrypointProgram = programs[entrypointFilePath]

  const globalIdentifiers = new Set([
    ...usedIdentifiers,
    ...getTopLevelIdentifiersInProgram(entrypointProgram).map(({ name }) => name)
  ])

  const globalObjId = create.identifier(getUniqueId(globalIdentifiers, INIT_OBJ_ID))
  const codeId = getUniqueId(globalIdentifiers, 'code')

  const [sourceModulesToImport, transpiledPrograms] = topoOrder.reduce(([sourceModules, outputPrograms], moduleName) => {
    const program = programs[moduleName]
    const [funcExpr, newSourceModules] = getTranspiler(
      program,
      context,
      null,
      globalIdentifiers
    ).transpile()

    return [
      new Set([
        ...sourceModules,
        ...newSourceModules
      ]),
      [
        ...outputPrograms,
        create.expressionStatement(
          create.assignmentExpression(
            create.memberExpression(
              create.memberExpression(
                globalObjId,
                'modules',
              ),
              moduleName,
              true
            ),
            create.callExpression(
              funcExpr,
              [globalObjId]
            )
          )
        )
      ]
    ]
  }, [new Set(), []] as [Set<string>, es.ExpressionStatement[]])

  const [{ body: { body: entrypointTranspiled } }, entrypointSourceModules] = getTranspiler(
    entrypointProgram,
    context,
    globalObjId,
    globalIdentifiers
  ).transpile()
  
  // Remove return statement
  entrypointTranspiled.pop()

  // Add builtins as necessary
  const builtInDecls = context.nativeStorage.evaller !== null ? [] : getBuiltinDecls(context, globalObjId)

  const newProgram = create.program([
    ...builtInDecls,
    ...transpiledPrograms,
    create.blockStatement([
      create.expressionStatement(
        create.assignmentExpression(
          create.memberExpression(
            create.memberExpression(
              globalObjId,
              'native'
              ),
            'evaller'
          ),
          create.arrowFunctionExpression([
              globalObjId,
              create.identifier(codeId)
            ],
            create.callExpression(
              create.identifier('eval'),
              [create.identifier(codeId)]
            )
          )
        )
      ),
      create.expressionStatement(
        create.identifier('undefined')
      ),
      ...entrypointTranspiled
    ])
  ])

  return {
    newProgram,
    sourceModulesToImport: new Set([
      ...sourceModulesToImport,
      ...entrypointSourceModules
    ]),
    globalObjId: globalObjId.name,
    codeId
  }
}