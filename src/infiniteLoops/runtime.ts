import type es from 'estree'

import { generate } from 'astring'
import createContext from '../createContext'
import { Chapter, Variant, type Context } from '../types'
import * as create from '../utils/ast/astCreator'
import { parse } from '../parser/parser'
import type { LinkerSuccess } from '../modules/preprocessor/linker'
import { NATIVE_STORAGE_ID } from '../constants'
import assert from '../utils/assert'
import { checkForInfiniteLoop } from './detect'
import { InfiniteLoopError } from './errors'
import {
  InfiniteLoopRuntimeFunctions as FunctionNames,
  transpileFilesToInfiniteLoop,
  prepareBuiltins
} from './instrument'
import * as st from './state'
import * as sym from './symbolic'

function checkTimeout(state: st.State) {
  if (state.hasTimedOut()) {
    throw new Error('timeout')
  }
}

/**
 * This function is run whenever a variable is being accessed.
 * If a variable has been added to state.variablesToReset, it will
 * be 'reset' (concretized and re-hybridized) here.
 */
function hybridize(originalValue: any, name: string, state: st.State) {
  if (typeof originalValue === 'function') {
    return originalValue
  }
  let value = originalValue
  if (state.variablesToReset.has(name)) {
    value = sym.deepConcretizeInplace(value)
  }
  return sym.hybridizeNamed(name, value)
}

/**
 * Function to keep track of assignment expressions.
 */
function saveVarIfHybrid(value: any, name: string, state: st.State) {
  state.variablesToReset.delete(name)
  if (sym.isHybrid(value)) {
    state.variablesModified.set(name, value)
  }
  return value
}

/**
 * Saves the boolean value if it is a hybrid, else set the
 * path to invalid.
 * Does not save in the path if the value is a boolean literal.
 */
function saveBoolIfHybrid(value: any, state: st.State) {
  if (sym.isHybrid(value) && value.type === 'value') {
    if (value.validity !== sym.Validity.Valid) {
      state.setInvalidPath()
      return sym.shallowConcretize(value)
    }
    if (value.symbolic.type !== 'Literal') {
      let theExpr: es.Expression = value.symbolic
      if (!value.concrete) {
        theExpr = value.negation ? value.negation : create.unaryExpression('!', theExpr)
      }
      state.savePath(theExpr)
    }
    return sym.shallowConcretize(value)
  } else {
    state.setInvalidPath()
    return value
  }
}

/**
 * If a function was passed as an argument we do not
 * check it for infinite loops. Wraps those functions
 * with a decorator that activates a flag in the state.
 */
function wrapArgIfFunction(arg: any, state: st.State) {
  if (typeof arg === 'function') {
    return (...args: any) => {
      state.functionWasPassedAsArgument = true
      return arg(...args)
    }
  }
  return arg
}

/**
 * For higher-order functions, we add the names of its parameters
 * that are functions to differentiate different combinations of
 * function invocations + parameters.
 *
 * e.g.
 * const f = x=>x;
 * const g = x=>x+1;
 * const h = f=>f(1);
 *
 * h(f) will have a different oracle name from h(g).
 */
function makeOracleName(name: string, args: [string, any][]) {
  let result = name
  for (const [n, v] of args) {
    if (typeof v === 'function') {
      result = `${result}_${n}:${v.name}`
    }
  }
  return result
}

function preFunction(name: string, args: [string, any][], state: st.State) {
  checkTimeout(state)
  // track functions which were passed as arguments in a different tracker
  const newName = state.functionWasPassedAsArgument ? '*' + name : makeOracleName(name, args)
  const [tracker, firstIteration] = state.enterFunction(newName)
  if (!firstIteration) {
    state.cleanUpVariables()
    state.saveArgsInTransition(args, tracker)
    if (!state.functionWasPassedAsArgument) {
      const previousIterations = tracker.slice(0, tracker.length - 1)
      checkForInfiniteLoopIfMeetsThreshold(previousIterations, state, name)
    }
  }
  tracker.push(state.newStackFrame(newName))

  // reset the flag
  state.functionWasPassedAsArgument = false
}

function returnFunction(value: any, state: st.State) {
  state.cleanUpVariables()
  if (!state.streamMode) state.returnLastFunction()
  return value
}

/**
 * Executed before the loop is entered to create a new iteration
 * tracker.
 */
function enterLoop(state: st.State) {
  state.loopStack.unshift([state.newStackFrame('loopRoot')])
}

// ignoreMe: hack to squeeze this inside the 'update' of for statements
function postLoop(state: st.State, ignoreMe?: any) {
  checkTimeout(state)
  const previousIterations = state.loopStack[0]
  checkForInfiniteLoopIfMeetsThreshold(
    previousIterations.slice(0, previousIterations.length - 1),
    state
  )
  state.cleanUpVariables()
  previousIterations.push(state.newStackFrame('loop'))
  return ignoreMe
}

/**
 * Always executed after a loop terminates, or breaks, to clean up
 * variables and pop the last iteration tracker.
 */
function exitLoop(state: st.State) {
  state.cleanUpVariables()
  state.exitLoop()
}

/**
 * If the number of iterations (given by the length
 * of stackPositions) is equal to a power of 2 times
 * the threshold, check these iterations for infinite loop.
 */
function checkForInfiniteLoopIfMeetsThreshold(
  stackPositions: number[],
  state: st.State,
  functionName?: string
) {
  let checkpoint = state.threshold
  while (checkpoint <= stackPositions.length) {
    if (stackPositions.length === checkpoint) {
      checkForInfiniteLoop(stackPositions, state, functionName)
    }
    checkpoint = checkpoint * 2
  }
}

function nothingFunction(..._args: any[]) {
  return nothingFunction
}

function trackLoc(loc: es.SourceLocation | undefined, state: st.State, ignoreMe?: () => any) {
  state.lastLocation = loc
  if (ignoreMe !== undefined) {
    return ignoreMe()
  }
}

const functions = {
  [FunctionNames.nothingFunction]: nothingFunction,
  [FunctionNames.concretize]: sym.shallowConcretize,
  [FunctionNames.hybridize]: hybridize,
  [FunctionNames.wrapArg]: wrapArgIfFunction,
  [FunctionNames.dummify]: sym.makeDummyHybrid,
  [FunctionNames.saveBool]: saveBoolIfHybrid,
  [FunctionNames.saveVar]: saveVarIfHybrid,
  [FunctionNames.preFunction]: preFunction,
  [FunctionNames.returnFunction]: returnFunction,
  [FunctionNames.postLoop]: postLoop,
  [FunctionNames.enterLoop]: enterLoop,
  [FunctionNames.exitLoop]: exitLoop,
  [FunctionNames.trackLoc]: trackLoc,
  [FunctionNames.evalB]: sym.evaluateHybridBinary,
  [FunctionNames.evalU]: sym.evaluateHybridUnary
}

/**
 * Tests the given program for infinite loops.
 * @param program Program to test.
 * @param previousProgramsStack Any code previously entered in the REPL & parsed into AST.
 * @returns SourceError if an infinite loop was detected, undefined otherwise.
 */
export function testForInfiniteLoop(
  res: Pick<LinkerSuccess, 'entrypointFilePath' | 'programs' | 'topoOrder'>,
  oldContext: Context
) {
  const context = createContext(Chapter.SOURCE_4, Variant.DEFAULT, undefined, undefined)
  const prelude = parse(context.prelude as string, context) as es.Program
  context.prelude = null
  context.nativeStorage.evaller = () => 0

  context.previousPrograms = [...oldContext.previousPrograms.slice(1), prelude]

  const newBuiltins = prepareBuiltins(context.nativeStorage.builtins)
  const transpiledProgram = transpileFilesToInfiniteLoop(res, context, true)
  assert(!!transpiledProgram, 'Infinite loop bundler should not throw errors')

  const instrumentedCode = generate(transpiledProgram)
  const state = new st.State()

  const sandboxedRun = new Function(
    'code',
    NATIVE_STORAGE_ID,
    // redeclare window so modules don't do anything funny like play sounds
    '{let window = {}; return eval(code)}'
  )

  try {
    sandboxedRun(instrumentedCode, {
      builtinsId: newBuiltins,
      functionsId: functions,
      stateId: state,
      loadedModules: oldContext.nativeStorage.loadedModules
    })
  } catch (error) {
    if (error instanceof InfiniteLoopError) {
      if (state.lastLocation !== undefined) {
        error.location = state.lastLocation
      }
      return error
    }

    // Programs that exceed the maximum call stack size are okay as long as they terminate.
    if (error instanceof RangeError && error.message === 'Maximum call stack size exceeded') {
      return undefined
    }
    throw error
  }
  return undefined
}
