import type es from 'estree'
import { getNativeTranspiler } from '../transpiler/transpileBundler'

import type { Node } from '../types'
import * as create from '../utils/ast/astCreator'
import { recursive, simple, WalkerCallback } from '../utils/walkers'
import { getUniqueId } from '../utils/uniqueIds'
import * as stdList from '../stdlib/list'
import * as sym from './symbolic'
import * as st from './state'

// const globalIds = {
//   builtinsId: 'builtins',
//   functionsId: '__InfLoopFns',
//   stateId: '__InfLoopState',
//   modulesId: '__modules'
// }
export const instrumenterInternals = {
  builtinsId: 'builtins',
  functionsId: '__InfLoopFns',
  stateId: '__InfLoopState'
}

type InstrumenterIDs = {
  [K in keyof typeof instrumenterInternals]: es.Identifier
}

enum FunctionNames {
  nothingFunction,
  concretize,
  hybridize,
  wrapArg,
  dummify,
  saveBool,
  saveVar,
  preFunction,
  returnFunction,
  postLoop,
  enterLoop,
  exitLoop,
  trackLoc,
  evalB,
  evalU
}

/**
 * Renames all variables in the program to differentiate shadowed variables and
 * variables declared with the same name but in different scopes.
 *
 * E.g. "function f(f)..." -> "function f_0(f_1)..."
 * @param predefined A table of [key: string, value:string], where variables named 'key' will be renamed to 'value'
 */
function unshadowVariables(program: Node, globalIds: InstrumenterIDs, predefined = {}) {
  // for (const name of Object.values(globalIds)) {
  //   predefined[name] = name
  // }
  const seenIds = new Set()
  const env = [predefined]
  const genId = (name: string) => {
    let count = 0
    while (seenIds.has(`${name}_${count}`)) count++
    const newName = `${name}_${count}`
    seenIds.add(newName)
    env[0][name] = newName
    return newName
  }
  const unshadowFunctionInner = (
    node: es.FunctionDeclaration | es.ArrowFunctionExpression | es.FunctionExpression,
    s: undefined,
    callback: WalkerCallback<undefined>
  ) => {
    env.unshift({ ...env[0] })
    for (const id of node.params as es.Identifier[]) {
      id.name = genId(id.name)
    }
    callback(node.body, undefined)
    env.shift()
  }
  const doStatements = (stmts: es.Statement[], callback: WalkerCallback<undefined>) => {
    for (const stmt of stmts) {
      if (stmt.type === 'FunctionDeclaration') {
        // do hoisting first
        if (stmt.id === null) {
          throw new Error(
            'Encountered a FunctionDeclaration node without an identifier. This should have been caught when parsing.'
          )
        }
        stmt.id.name = genId(stmt.id.name)
      } else if (stmt.type === 'VariableDeclaration') {
        for (const decl of stmt.declarations) {
          decl.id = decl.id as es.Identifier
          const newName = genId(decl.id.name)
          decl.id.name = newName
        }
      }
    }
    for (const stmt of stmts) {
      callback(stmt, undefined)
    }
  }
  recursive(program, [{}], {
    BlockStatement(node: es.BlockStatement, s: undefined, callback: WalkerCallback<undefined>) {
      env.unshift({ ...env[0] })
      doStatements(node.body, callback)
      env.shift()
    },
    VariableDeclarator(
      node: es.VariableDeclarator,
      s: undefined,
      callback: WalkerCallback<undefined>
    ) {
      node.id = node.id as es.Identifier
      if (node.init) {
        callback(node.init, s)
      }
    },
    FunctionDeclaration(
      node: es.FunctionDeclaration,
      s: undefined,
      callback: WalkerCallback<undefined>
    ) {
      // note: params can shadow function name
      env.unshift({ ...env[0] })
      for (const id of node.params as es.Identifier[]) {
        id.name = genId(id.name)
      }
      callback(node.body, undefined)
      env.shift()
    },
    ForStatement(node: es.ForStatement, s: undefined, callback: WalkerCallback<undefined>) {
      env.unshift({ ...env[0] })
      if (node.init?.type === 'VariableDeclaration') doStatements([node.init], callback)
      if (node.test) callback(node.test, s)
      if (node.update) callback(node.update, s)
      callback(node.body, s)
      env.shift()
    },
    ArrowFunctionExpression: unshadowFunctionInner,
    FunctionExpression: unshadowFunctionInner,
    Identifier(node: es.Identifier, _s: undefined, _callback: WalkerCallback<undefined>) {
      if (env[0][node.name]) {
        node.name = env[0][node.name]
      } else {
        create.mutateToMemberExpression(
          node,
          globalIds.functionsId,
          create.literal(FunctionNames.nothingFunction)
        )
        ;(node as any).computed = true
      }
    },
    AssignmentExpression(
      node: es.AssignmentExpression,
      s: undefined,
      callback: WalkerCallback<undefined>
    ) {
      callback(node.left, s)
      callback(node.right, s)
    },
    TryStatement(node: es.TryStatement, s: undefined, callback: WalkerCallback<undefined>) {
      if (!node.finalizer) return // should not happen
      env.unshift({ ...env[0] })
      doStatements(node.block.body, callback)
      doStatements(node.finalizer.body, callback)
      env.shift()
    }
  })
}

/**
 * Returns the original name of the variable before
 * it was changed during the code instrumentation process.
 */
export function getOriginalName(name: string) {
  if (/^anon[0-9]+$/.exec(name)) {
    return '(anonymous)'
  }
  let cutAt = name.length - 1
  while (name.charAt(cutAt) !== '_') {
    cutAt--
    if (cutAt < 0) return '(error)'
  }
  return name.slice(0, cutAt)
}

function callFunction(fun: FunctionNames, globalIds: InstrumenterIDs) {
  return create.memberExpression(globalIds.functionsId, fun)
}

/**
 * Wrap each argument in every call expression.
 *
 * E.g. "f(x,y)" -> "f(wrap(x), wrap(y))".
 * Ensures we do not test functions passed as arguments
 * for infinite loops.
 */
function wrapCallArguments(program: es.Program, globalIds: InstrumenterIDs) {
  simple(program, {
    CallExpression(node: es.CallExpression) {
      if (node.callee.type === 'MemberExpression') return

      node.arguments = node.arguments.map(arg =>
        create.callExpression(callFunction(FunctionNames.wrapArg, globalIds), [
          arg,
          globalIds.stateId
        ])
      )
    }
  })
}

/**
 * Turn all "is_null(x)" calls to "is_null(x, stateId)" to
 * facilitate checking of infinite streams in stream mode.
 */
function addStateToIsNull(program: es.Program, globalIds: InstrumenterIDs) {
  simple(program, {
    CallExpression(node: es.CallExpression) {
      if (node.callee.type === 'Identifier' && node.callee.name === 'is_null_0') {
        node.arguments.push(globalIds.stateId)
      }
    }
  })
}

/**
 * Changes logical expressions to the corresponding conditional.
 * Reduces the number of types of expressions we have to consider
 * for the rest of the code transformations.
 *
 * E.g. "x && y" -> "x ? y : false"
 */
function transformLogicalExpressions(program: es.Program) {
  simple(program, {
    LogicalExpression(node: es.LogicalExpression) {
      if (node.operator === '&&') {
        create.mutateToConditionalExpression(node, node.left, node.right, create.literal(false))
      } else {
        create.mutateToConditionalExpression(node, node.left, create.literal(true), node.right)
      }
    }
  })
}

/**
 * Changes -ary operations to functions that accept hybrid values as arguments.
 * E.g. "1+1" -> "functions.evalB('+',1,1)"
 */
function hybridizeBinaryUnaryOperations(program: Node, globalIds: InstrumenterIDs) {
  simple(program, {
    BinaryExpression(node: es.BinaryExpression) {
      const { operator, left, right } = node
      create.mutateToCallExpression(node, callFunction(FunctionNames.evalB, globalIds), [
        create.literal(operator),
        left,
        right
      ])
    },
    UnaryExpression(node: es.UnaryExpression) {
      const { operator, argument } = node as es.UnaryExpression
      create.mutateToCallExpression(node, callFunction(FunctionNames.evalU, globalIds), [
        create.literal(operator),
        argument
      ])
    }
  })
}

function hybridizeVariablesAndLiterals(program: Node, globalIds: InstrumenterIDs) {
  recursive(program, true, {
    Identifier(node: es.Identifier, state: boolean, _callback: WalkerCallback<boolean>) {
      if (state) {
        create.mutateToCallExpression(node, callFunction(FunctionNames.hybridize, globalIds), [
          create.identifier(node.name),
          create.literal(node.name),
          globalIds.stateId
        ])
      }
    },
    Literal(node: es.Literal, state: boolean, _callback: WalkerCallback<boolean>) {
      if (state && (typeof node.value === 'boolean' || typeof node.value === 'number')) {
        create.mutateToCallExpression(node, callFunction(FunctionNames.dummify, globalIds), [
          create.literal(node.value)
        ])
      }
    },
    CallExpression(node: es.CallExpression, state: boolean, callback: WalkerCallback<boolean>) {
      // ignore callee
      for (const arg of node.arguments) {
        callback(arg, state)
      }
    },
    MemberExpression(node: es.MemberExpression, state: boolean, callback: WalkerCallback<boolean>) {
      if (!node.computed) return

      callback(node.object, false)
      callback(node.property, false)

      node.object = create.callExpression(
        callFunction(FunctionNames.concretize, globalIds),
        [node.object as es.Expression],
        node.loc
      )

      node.property = create.callExpression(
        callFunction(FunctionNames.concretize, globalIds),
        [node.property as es.Expression],
        node.loc
      )
    }
  })
}

/**
 * Wraps the RHS of variable assignment with a function to track it.
 * E.g. "x = x + 1;" -> "x = saveVar(x + 1, 'x', state)".
 * saveVar should return the result of "x + 1".
 *
 * For assignments to elements of arrays we concretize the RHS.
 * E.g. "a[1] = y;" -> "a[1] = concretize(y);"
 */
function trackVariableAssignment(program: Node, globalIds: InstrumenterIDs) {
  simple(program, {
    AssignmentExpression(node: es.AssignmentExpression) {
      if (node.left.type === 'Identifier') {
        node.right = create.callExpression(callFunction(FunctionNames.saveVar, globalIds), [
          node.right,
          create.literal(node.left.name),
          globalIds.stateId
        ])
      } else if (node.left.type === 'MemberExpression') {
        node.right = create.callExpression(callFunction(FunctionNames.concretize, globalIds), [
          { ...node.right }
        ])
      }
    }
  })
}

/**
 * Replaces the test of the node with a function to track the result in the state.
 *
 * E.g. "x===0 ? 1 : 0;" -> "saveBool(x === 0, state) ? 1 : 0;".
 * saveBool should return the result of "x === 0"
 */
function saveTheTest(
  node: es.IfStatement | es.ConditionalExpression | es.WhileStatement | es.ForStatement,
  globalIds: InstrumenterIDs
) {
  if (node.test === null || node.test === undefined) {
    return
  }
  const newTest = create.callExpression(callFunction(FunctionNames.saveBool, globalIds), [
    node.test,
    globalIds.stateId
  ])
  node.test = newTest
}

/**
 * Mutates a node in-place, turning it into a block statement.
 * @param node Node to mutate.
 * @param prepend Optional statement to prepend in the result.
 * @param append Optional statement to append in the result.
 */
function inPlaceEnclose(node: es.Statement, prepend?: es.Statement, append?: es.Statement) {
  const shallowCopy = { ...node }
  node.type = 'BlockStatement'
  node = node as es.BlockStatement
  node.body = [shallowCopy]
  if (prepend !== undefined) {
    node.body.unshift(prepend)
  }
  if (append !== undefined) {
    node.body.push(append)
  }
}

/**
 * Add tracking to if statements and conditional expressions in the state using saveTheTest.
 */
function trackIfStatements(program: Node, globalIds: InstrumenterIDs) {
  const theFunction = (node: es.IfStatement | es.ConditionalExpression) =>
    saveTheTest(node, globalIds)
  simple(program, { IfStatement: theFunction, ConditionalExpression: theFunction })
}

/**
 * Tracks loop iterations by adding saveTheTest, postLoop functions.
 * postLoop will be executed after the body (and the update if it is a for loop).
 * Also adds enter/exitLoop before/after the loop.
 *
 * E.g. "for(let i=0;i<10;i=i+1) {display(i);}"
 *      -> "enterLoop(state);
 *          for(let i=0;i<10; postLoop(state, i=i+1)) {display(i);};
 *          exitLoop(state);"
 * Where postLoop should return the value of its (optional) second argument.
 */
function trackLoops(program: Node, globalIds: InstrumenterIDs) {
  const makeCallStatement = (name: FunctionNames, args: es.Expression[]) =>
    create.expressionStatement(create.callExpression(callFunction(name, globalIds), args))
  const stateExpr = globalIds.stateId

  simple(program, {
    WhileStatement: (node: es.WhileStatement) => {
      saveTheTest(node, globalIds)
      inPlaceEnclose(node.body, undefined, makeCallStatement(FunctionNames.postLoop, [stateExpr]))
      inPlaceEnclose(
        node,
        makeCallStatement(FunctionNames.enterLoop, [stateExpr]),
        makeCallStatement(FunctionNames.exitLoop, [stateExpr])
      )
    },
    ForStatement: (node: es.ForStatement) => {
      saveTheTest(node, globalIds)
      const theUpdate = node.update ? node.update : create.identifier('undefined')
      node.update = create.callExpression(callFunction(FunctionNames.postLoop, globalIds), [
        stateExpr,
        theUpdate
      ])
      inPlaceEnclose(
        node,
        makeCallStatement(FunctionNames.enterLoop, [stateExpr]),
        makeCallStatement(FunctionNames.exitLoop, [stateExpr])
      )
    }
  })
}

/**
 * Tracks function iterations by adding preFunction and returnFunction functions.
 * preFunction is prepended to every function body, and returnFunction is used to
 * wrap the argument of return statements.
 *
 * E.g. "function f(x) {return x;}"
 *      -> "function f(x) {
 *            preFunction('f',[x], state);
 *            return returnFunction(x, state);
 *         }"
 * where returnFunction should return its first argument 'x'.
 */
function trackFunctions(program: Node, globalIds: InstrumenterIDs) {
  const preFunction = (name: string, params: es.Pattern[]) => {
    const args = params
      .filter(x => x.type === 'Identifier')
      .map(x => (x as es.Identifier).name)
      .map(x => create.arrayExpression([create.literal(x), create.identifier(x)]))

    return create.expressionStatement(
      create.callExpression(callFunction(FunctionNames.preFunction, globalIds), [
        create.literal(name),
        create.arrayExpression(args),
        globalIds.stateId
      ])
    )
  }

  let counter = 0
  const anonFunction = (node: es.ArrowFunctionExpression | es.FunctionExpression) => {
    if (node.body.type !== 'BlockStatement') {
      create.mutateToReturnStatement(node.body, { ...node.body })
    }
    inPlaceEnclose(node.body as es.Statement, preFunction(`anon${counter++}`, node.params))
  }
  simple(program, {
    ArrowFunctionExpression: anonFunction,
    FunctionExpression: anonFunction,
    FunctionDeclaration(node: es.FunctionDeclaration) {
      if (node.id === null) {
        throw new Error(
          'Encountered a FunctionDeclaration node without an identifier. This should have been caught when parsing.'
        )
      }
      const name = node.id.name
      inPlaceEnclose(node.body, preFunction(name, node.params))
    }
  })
  simple(program, {
    ReturnStatement(node: es.ReturnStatement) {
      const hasNoArgs = node.argument === null || node.argument === undefined
      const arg = hasNoArgs ? create.identifier('undefined') : (node.argument as es.Expression)
      const argsForCall = [arg, globalIds.stateId]
      node.argument = create.callExpression(
        callFunction(FunctionNames.returnFunction, globalIds),
        argsForCall
      )
    }
  })
}

function builtinsToStmts(builtins: Iterable<string>, globalIds: InstrumenterIDs) {
  const makeDecl = (name: string) =>
    create.declaration(
      name,
      'const',
      create.callExpression(create.memberExpression(globalIds.builtinsId, 'get'), [
        create.literal(name)
      ])
    )
  return [...builtins].map(makeDecl)
}

/**
 * Make all variables in the 'try' block function-scoped so they
 * can be accessed in the 'finally' block
 */
function toVarDeclaration(stmt: es.Statement) {
  simple(stmt, {
    VariableDeclaration(node: es.VariableDeclaration) {
      node.kind = 'var'
    }
  })
}

/**
 * There may have been other programs run in the REPL. This hack
 * 'combines' the other programs and the current program into a single
 * large program by enclosing the past programs in 'try' blocks, and the
 * current program in a 'finally' block. Any errors (including detected
 * infinite loops) in the past code will be ignored in the empty 'catch'
 * block.
 */
function wrapOldCode(current: es.Program, toWrap: es.Statement[]) {
  for (const stmt of toWrap) {
    toVarDeclaration(stmt)
  }
  const tryStmt: es.TryStatement = {
    type: 'TryStatement',
    block: create.blockStatement([...toWrap]),
    handler: {
      type: 'CatchClause',
      param: create.identifier('e'),
      body: create.blockStatement([])
    },
    finalizer: create.blockStatement([...(current.body as es.Statement[])])
  }
  current.body = [tryStmt]
}

function makePositions(position: es.Position) {
  return create.objectExpression([
    create.property('line', create.literal(position.line)),
    create.property('column', create.literal(position.column))
  ])
}

function savePositionAsExpression(loc: es.SourceLocation | undefined | null) {
  if (loc !== undefined && loc !== null) {
    return create.objectExpression([
      create.property('start', makePositions(loc.start)),
      create.property('end', makePositions(loc.end))
    ])
  } else {
    return create.identifier('undefined')
  }
}

/**
 * Wraps every callExpression and prepends every loop body
 * with a function that saves the callExpression/loop's SourceLocation
 * (line number etc) in the state. This location will be used in the
 * error given to the user.
 *
 * E.g. "f(x);" -> "trackLoc({position object}, state, ()=>f(x))".
 * where trackLoc should return the result of "(()=>f(x))();".
 */
function trackLocations(program: es.Program, globalIds: InstrumenterIDs) {
  // Note: only add locations for most recently entered code
  const trackerFn = callFunction(FunctionNames.trackLoc, globalIds)
  const stateExpr = globalIds.stateId
  const doLoops = (
    node: es.ForStatement | es.WhileStatement,
    _state: undefined,
    _callback: WalkerCallback<undefined>
  ) => {
    inPlaceEnclose(
      node.body,
      create.expressionStatement(
        create.callExpression(trackerFn, [savePositionAsExpression(node.loc), stateExpr])
      )
    )
  }
  recursive(program, undefined, {
    CallExpression(
      node: es.CallExpression,
      _state: undefined,
      _callback: WalkerCallback<undefined>
    ) {
      if (node.callee.type === 'MemberExpression') return
      const copy: es.CallExpression = { ...node }
      const lazyCall = create.arrowFunctionExpression([], copy)
      create.mutateToCallExpression(node, trackerFn, [
        savePositionAsExpression(node.loc),
        stateExpr,
        lazyCall
      ])
    },
    ForStatement: doLoops,
    WhileStatement: doLoops
  })
}

function returnInvalidIfNumeric(val: any, validity = sym.Validity.NoSmt) {
  if (typeof val === 'number') {
    const result = sym.makeDummyHybrid(val)
    result.validity = validity
    return result
  } else {
    return val
  }
}

const builtinSpecialCases = {
  is_null(maybeHybrid: any, state?: st.State) {
    const xs = sym.shallowConcretize(maybeHybrid)
    const conc = stdList.is_null(xs)
    const theTail = stdList.is_pair(xs) ? xs[1] : undefined
    const isStream = typeof theTail === 'function'
    if (state && isStream) {
      const lastFunction = state.getLastFunctionName()
      if (state.streamMode === true && state.streamLastFunction === lastFunction) {
        // heuristic to make sure we are at the same is_null call
        testIfInfiniteStream(sym.shallowConcretize(theTail()), state)
      } else {
        let count = state.streamCounts.get(lastFunction)
        if (count === undefined) {
          count = 1
        }
        if (count > state.streamThreshold) {
          state.streamMode = true
          state.streamLastFunction = lastFunction
        }
        state.streamCounts.set(lastFunction, count + 1)
      }
    } else {
      return conc
    }
    return
  },
  // mimic behaviour without printing
  display: (...x: any[]) => x[0],
  display_list: (...x: any[]) => x[0]
}

/**
 * Test if stream is infinite. May destructively change the program
 * environment. If it is not infinite, throw a timeout error.
 */

function testIfInfiniteStream(stream: any, state: st.State) {
  let next = stream
  for (let i = 0; i <= state.threshold; i++) {
    if (stdList.is_null(next)) {
      break
    } else {
      const nextTail = stdList.is_pair(next) ? next[1] : undefined
      if (typeof nextTail === 'function') {
        next = sym.shallowConcretize(nextTail())
      } else {
        break
      }
    }
  }
  throw new Error('timeout')
}

export function prepareBuiltins(oldBuiltins: Map<string, any>) {
  const nonDetFunctions = ['get_time', 'math_random']
  const newBuiltins = new Map<string, any>()
  for (const [name, fun] of oldBuiltins) {
    const specialCase = builtinSpecialCases[name]
    if (specialCase !== undefined) {
      newBuiltins.set(name, specialCase)
    } else {
      const functionValidity = nonDetFunctions.includes(name)
        ? sym.Validity.NoCycle
        : sym.Validity.NoSmt
      newBuiltins.set(name, (...args: any[]) => {
        const validityOfArgs = args.filter(sym.isHybrid).map(x => x.validity)
        const mostInvalid = Math.max(functionValidity, ...validityOfArgs)
        return returnInvalidIfNumeric(fun(...args.map(sym.shallowConcretize)), mostInvalid)
      })
    }
  }
  newBuiltins.set('undefined', undefined)
  return newBuiltins
}

const getInstrumenterInternals = (usedIdentifiers: Set<string>) =>
  Object.entries(instrumenterInternals).reduce(
    (res, [key, value]) => ({
      ...res,
      [key]: create.identifier(getUniqueId(usedIdentifiers, value))
    }),
    {} as InstrumenterIDs
  )

export const transpileFilesToInfiniteLoop = getNativeTranspiler(
  (program, context, nativeId, isEntrypoint) => {
    const innerProgram = { ...program }
    const instrumenterIds = getInstrumenterInternals(new Set())
    const predefined = {
      [nativeId.name]: nativeId.name
    }

    for (const { name } of Object.values(instrumenterIds)) {
      predefined[name] = name
    }

    for (const node of program.body) {
      if (node.type === 'ImportDeclaration') {
        for (const specifier of node.specifiers) {
          const { name } = specifier.local
          predefined[name] = name
        }
      }
    }

    if (isEntrypoint) {
      const newBuiltins = prepareBuiltins(context.nativeStorage.builtins)
      for (const toWrap of context.previousPrograms) {
        wrapOldCode(program, toWrap.body as es.Statement[])
      }
      wrapOldCode(program, builtinsToStmts(newBuiltins.keys(), instrumenterIds))
    }

    unshadowVariables(program, instrumenterIds, predefined)
    transformLogicalExpressions(program)
    hybridizeBinaryUnaryOperations(program, instrumenterIds)
    hybridizeVariablesAndLiterals(program, instrumenterIds)
    // tracking functions: add functions to record runtime data.

    trackVariableAssignment(program, instrumenterIds)
    trackIfStatements(program, instrumenterIds)
    trackLoops(program, instrumenterIds)
    trackFunctions(program, instrumenterIds)
    trackLocations(innerProgram, instrumenterIds)
    addStateToIsNull(program, instrumenterIds)
    wrapCallArguments(program, instrumenterIds)

    const internalsDeclaration: es.VariableDeclaration = {
      type: 'VariableDeclaration',
      kind: 'const',
      declarations: [
        {
          type: 'VariableDeclarator',
          id: {
            type: 'ObjectPattern',
            properties: Object.entries(instrumenterIds).map(([local, declared]) => ({
              type: 'Property',
              key: create.identifier(local),
              value: declared,
              kind: 'init',
              method: false,
              shorthand: false,
              computed: false
            }))
          },
          init: nativeId
        }
      ]
    }

    return [[internalsDeclaration], program.body]
  }
)

export { FunctionNames as InfiniteLoopRuntimeFunctions }
