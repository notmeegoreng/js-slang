import { Program } from "estree";

import { createContext,IOptions } from "..";
import { NativeRunner, NativeRunnerArgs } from "../runner/runner";
import { ProgramTranspiler } from "../transpiler/baseTranspiler";
import { Chapter, Variant } from "../types";
import { InfiniteLoopError } from "./errors";
import { functions } from "./runtime";
import * as st from './state'

class InfiniteLoopProgramTranspiler extends ProgramTranspiler {

}

class InfiniteLoopRunner extends NativeRunner<InfiniteLoopError | undefined> {
  constructor(
    programs: Record<string, Program>,
    topoOrder: string[],
    options: IOptions,
  ) {
    const context = createContext(Chapter.SOURCE_4, Variant.DEFAULT, undefined, undefined)
    super(programs, topoOrder, context, options, (...args) => new InfiniteLoopProgramTranspiler(...args))
  }

  protected async runNative({
    transpiledCode,
    evaler: sandboxedRun,
    initObj
  }: NativeRunnerArgs) {
    const state = new st.State()

    const newInitObj = {
      ...initObj,
      state,
      functions,
    }
    
    const instrumentedCode = `
    {
      let window = {};
      ${transpiledCode}
    }
    `

    
    try {
      await sandboxedRun(newInitObj, instrumentedCode)
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
}

export default function testForInfiniteLoop(
  programs: Record<string, Program>,
  topoOrder: string[],
  options: IOptions,
) {
  return new InfiniteLoopRunner(
    programs,
    topoOrder,
    options
  ).run()
}