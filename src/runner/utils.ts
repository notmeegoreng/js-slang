/* eslint-disable @typescript-eslint/no-unused-vars */
import type { DebuggerStatement, Program } from 'estree'

import type { IOptions, Result } from '..'
import { areBreakpointsSet } from '../stdlib/inspector'
import type { Context, RecursivePartial, Variant } from '../types'
import { simple } from '../utils/walkers'

// Context Utils

/**
 * Small function to determine the variant to be used
 * by a program, as both context and options can have
 * a variant. The variant provided in options will
 * have precedence over the variant provided in context.
 *
 * @param context The context of the program.
 * @param options Options to be used when
 *                running the program.
 *
 * @returns The variant that the program is to be run in
 */
export function determineVariant(context: Context, options: RecursivePartial<IOptions>): Variant {
  if (options.variant) {
    return options.variant
  } else {
    return context.variant
  }
}

export function determineExecutionMethod(
  theOptions: IOptions,
  context: Context,
  programs: Record<string, Program>,
  verboseErrors: boolean
): void {
  if (theOptions.executionMethod !== 'auto') {
    context.executionMethod = theOptions.executionMethod
    return
  }

  if (context.executionMethod !== 'auto') {
    return
  }

  let isNativeRunnable
  if (verboseErrors || areBreakpointsSet()) {
    isNativeRunnable = false
  } else if (theOptions.executionMethod === 'auto') {
    for (const program of Object.values(programs)) {
      isNativeRunnable = true
      simple(program, {
        DebuggerStatement(node: DebuggerStatement) {
          isNativeRunnable = false
        }
      })

      if (!isNativeRunnable) break
    }
  }
  context.executionMethod = isNativeRunnable ? 'native' : 'cse-machine'
}

// AST Utils

export const resolvedErrorPromise = Promise.resolve({ status: 'error' } as Result)
