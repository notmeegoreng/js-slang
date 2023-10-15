import { Program } from "estree";
import _ from "lodash";

import { Context, IOptions, Result } from "..";
import { ExceptionError } from "../errors/errors";
import { CircularImportError } from "../errors/localImportErrors";
import { RuntimeSourceError } from "../errors/runtimeSourceError";
import { TimeoutError } from "../errors/timeoutErrors";
import { isPotentialInfiniteLoop } from "../infiniteLoops/errors";
import testForInfiniteLoop from "../infiniteLoops/transpiler";
import { defaultImportOptions } from "../modules/moduleTypes";
import parseProgramsAndConstructImportGraph from "../modules/preprocessor/analyzer";
import SourceProgramTranspiler from "../transpiler/sourceTranspiler";
import { RecursivePartial, Variant } from "../types";
import { toSourceError } from "./errors";
import { NativeRunner, NativeRunnerArgs } from "./runner";
import { resolvedErrorPromise } from "./utils";

const DEFAULT_SOURCE_OPTIONS: Readonly<IOptions> = {
  scheduler: 'async',
  steps: 1000,
  stepLimit: -1,
  executionMethod: 'auto',
  variant: Variant.DEFAULT,
  originalMaxExecTime: 1000,
  useSubst: false,
  isPrelude: false,
  throwInfiniteLoops: true,
  envSteps: -1,
  importOptions: defaultImportOptions
}

export default class NativeSourceRunner extends NativeRunner {
  constructor(
    programs: Record<string, Program>,
    topoOrder: string[],
    context: Context,
    options: IOptions,
  ) {
    super(programs, topoOrder, context, options, (...args) => new SourceProgramTranspiler(...args))
  }

  protected async runNative({
    evaler,
    initObj,
    sourceMapJson,
    transpiledCode,
  }: NativeRunnerArgs): Promise<Result> {
    try {
      const value = evaler(initObj, transpiledCode)
      return {
        status: 'finished',
        value,
        context: this.context,
      }
    } catch (error) {
      const isDefaultVariant = this.options.variant === undefined || this.options.variant === Variant.DEFAULT
      if (isDefaultVariant && isPotentialInfiniteLoop(error)) {
        const detectedInfiniteLoop = await testForInfiniteLoop(
          this.programs,
          this.topoOrder,
          this.context,
          this.options
        )

        if (detectedInfiniteLoop !== undefined) {
          if (this.options.throwInfiniteLoops) {
            this.context.errors.push(detectedInfiniteLoop)
            return resolvedErrorPromise
          } else {
            error.infiniteLoopError = detectedInfiniteLoop
            if (error instanceof ExceptionError) {
              ;(error.error as any).infiniteLoopError = detectedInfiniteLoop
            }
          }
        }
      }
      if (error instanceof RuntimeSourceError) {
        this.context.errors.push(error)
        if (error instanceof TimeoutError) {
          this.context.isPreviousCodeTimeoutError = true
        }
        return resolvedErrorPromise
      }
      if (error instanceof ExceptionError) {
        // if we know the location of the error, just throw it
        if (error.location.start.line !== -1) {
          this.context.errors.push(error)
          return resolvedErrorPromise
        } else {
          error = error.error // else we try to get the location from source map
        }
      }

      const sourceError = await toSourceError(error, sourceMapJson)
      this.context.errors.push(sourceError)
      return resolvedErrorPromise
    }
  }
}

export async function runFilesInContext(
  fileGetter: (path: string) => Promise<string | undefined>,
  entrypointFilePath: string,
  context: Context,
  options: RecursivePartial<IOptions> = {}
) {

  const theOptions = _.merge({ ...DEFAULT_SOURCE_OPTIONS }, options)
  const importGraphResult = await parseProgramsAndConstructImportGraph(
    fileGetter,
    entrypointFilePath,
    context,
    theOptions.importOptions.analysisOptions
  )

  // Return 'undefined' if there are errors while parsing.
  if (!importGraphResult || context.errors.length !== 0) {
    return resolvedErrorPromise
  }

  const { programs, importGraph } = importGraphResult
  // Check for circular imports.
  const topologicalOrderResult = importGraph.getTopologicalOrder()
  if (!topologicalOrderResult.isValidTopologicalOrderFound) {
    context.errors.push(new CircularImportError(topologicalOrderResult.firstCycleFound))
    return resolvedErrorPromise
  }

  const topoOrder = topologicalOrderResult.topologicalOrder
  if (topoOrder.length === 0) {
    topoOrder.push(entrypointFilePath)
  }

  const runner = new NativeSourceRunner(
    programs,
    topoOrder,
    context,
    theOptions
  )

  return runner.run()
}