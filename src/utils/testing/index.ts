import { generate } from 'astring'
import type { MockedFunction } from 'jest-mock'

import type { Program } from 'estree'
import createContext, { defineBuiltin } from '../../createContext'
import { transpileToGPU } from '../../gpu/gpu'
import { parseError, runInContext } from '../..'
import { transpileToLazy } from '../../lazy/lazy'
import type { ImportOptions } from '../../modules/moduleTypes'
import { parse } from '../../parser/parser'
import { transpile } from '../../transpiler/transpiler'
import {
  Chapter,
  type Context,
  type CustomBuiltIns,
  type SourceError,
  type Value,
  Variant,
  type Finished,
  type Result
} from '../../types'
import { mockContext } from '../../mocks/context'
import { stringify } from '../stringify'

export interface CodeSnippetTestCase {
  name: string
  snippet: string
  value: any
  errors: SourceError[]
}

export interface TestContext extends Context {
  displayResult: string[]
  promptResult: string[]
  alertResult: string[]
  visualiseListResult: Value[]
}

export interface TestBuiltins {
  [builtinName: string]: any
}

interface TestResult {
  code: string
  displayResult: string[]
  alertResult: string[]
  visualiseListResult: any[]
  errors?: SourceError[]
  numErrors: number
  parsedErrors: string
  resultStatus: string
  result: Value
}

export interface TestOptions {
  context?: TestContext
  chapter?: Chapter
  variant?: Variant
  testBuiltins?: TestBuiltins
  native?: boolean
  showTranspiledCode?: boolean
  showErrorJSON?: boolean
  importOptions?: Partial<ImportOptions>
}

export function createTestContext({
  context,
  chapter = Chapter.SOURCE_1,
  variant = Variant.DEFAULT,
  testBuiltins = {}
}: {
  context?: TestContext
  chapter?: Chapter
  variant?: Variant
  testBuiltins?: TestBuiltins
} = {}): TestContext {
  if (context !== undefined) {
    return context
  } else {
    const testContext: TestContext = {
      ...createContext(chapter, variant, [], undefined, {
        rawDisplay: (str1, str2, _externalContext) => {
          testContext.displayResult.push((str2 === undefined ? '' : str2 + ' ') + str1)
          return str1
        },
        prompt: (str, _externalContext) => {
          testContext.promptResult.push(str)
          return null
        },
        alert: (str, _externalContext) => {
          testContext.alertResult.push(str)
        },
        visualiseList: value => {
          testContext.visualiseListResult.push(value)
        }
      } as CustomBuiltIns),
      displayResult: [],
      promptResult: [],
      alertResult: [],
      visualiseListResult: []
    }
    Object.entries(testBuiltins).forEach(([key, value]) => defineBuiltin(testContext, key, value))

    return testContext
  }
}

async function testInContext(code: string, options: TestOptions): Promise<TestResult> {
  const interpretedTestContext = createTestContext(options)
  const scheduler = 'preemptive'
  const getTestResult = (context: TestContext, result: Result) => {
    const testResult = {
      code,
      displayResult: context.displayResult,
      alertResult: context.alertResult,
      visualiseListResult: context.visualiseListResult,
      numErrors: context.errors.length,
      parsedErrors: parseError(context.errors),
      resultStatus: result.status,
      result: result.status === 'finished' ? result.value : undefined
    }
    if (options.showErrorJSON) {
      testResult['errors'] = context.errors
    }
    return testResult
  }
  const interpretedResult = getTestResult(
    interpretedTestContext,
    await runInContext(code, interpretedTestContext, {
      scheduler,
      executionMethod: 'cse-machine',
      variant: options.variant
    })
  )
  if (options.native) {
    const nativeTestContext = createTestContext(options)
    let pretranspiled: string = ''
    let transpiled: string = ''
    const parsed = parse(code, nativeTestContext)!
    // Reset errors in context so as not to interfere with actual run.
    nativeTestContext.errors = []
    if (parsed === undefined) {
      pretranspiled = 'parseError'
    } else {
      // Mutates program
      switch (options.variant) {
        case Variant.GPU:
          transpileToGPU(parsed)
          pretranspiled = generate(parsed)
          break
        case Variant.LAZY:
          transpileToLazy(parsed)
          pretranspiled = generate(parsed)
          break
      }
      try {
        ;({ transpiled } = transpile(parsed, nativeTestContext))
        // replace declaration of builtins since they're repetitive
        transpiled = transpiled.replace(/\n  const \w+ = nativeStorage\..*;/g, '')
        transpiled = transpiled.replace(/\n\s*const \w+ = .*\.operators\..*;/g, '')
      } catch {
        transpiled = 'parseError'
      }
    }
    const nativeResult = getTestResult(
      nativeTestContext,
      await runInContext(code, nativeTestContext, {
        scheduler,
        executionMethod: 'native',
        variant: options.variant
      })
    )
    const propertiesThatShouldBeEqual = [
      'code',
      'displayResult',
      'alertResult',
      'parsedErrors',
      'result'
    ]
    const diff = {}
    for (const property of propertiesThatShouldBeEqual) {
      const nativeValue = stringify(nativeResult[property])
      const interpretedValue = stringify(interpretedResult[property])
      if (nativeValue !== interpretedValue) {
        diff[property] = `native:${nativeValue}\ninterpreted:${interpretedValue}`
      }
    }
    if (options.showTranspiledCode) {
      return { ...interpretedResult, ...diff, pretranspiled, transpiled } as TestResult
    } else {
      return { ...interpretedResult, ...diff } as TestResult
    }
  } else {
    return interpretedResult
  }
}

export async function testSuccess(code: string, options: TestOptions = { native: false }) {
  const testResult = await testInContext(code, options)
  expect(testResult.parsedErrors).toBe('')
  expect(testResult.resultStatus).toBe('finished')
  return testResult
}

export async function testFailure(code: string, options: TestOptions = { native: false }) {
  const testResult = await testInContext(code, options)
  expect(testResult.numErrors).not.toEqual(0)
  expect(testResult.resultStatus).toBe('error')
  return testResult
}

export function snapshot<T extends { [P in keyof TestResult]: any }>(
  propertyMatchers: Partial<T>,
  snapshotName?: string
): (testResult: TestResult) => TestResult
export function snapshot(
  snapshotName?: string,
  arg2?: string
): (testResult: TestResult) => TestResult
export function snapshot(arg1?: any, arg2?: any): (testResult: TestResult) => TestResult {
  if (arg2) {
    return testResult => {
      expect(testResult).toMatchSnapshot(arg1!, arg2)
      return testResult
    }
  } else if (arg1) {
    return testResult => {
      expect(testResult).toMatchSnapshot(arg1!)
      return testResult
    }
  } else {
    return testResult => {
      return testResult
    }
  }
}

export function snapshotSuccess(code: string, options: TestOptions, snapshotName?: string) {
  return testSuccess(code, options).then(snapshot(snapshotName))
}

export function snapshotFailure(code: string, options: TestOptions, snapshotName: string) {
  return testFailure(code, options).then(snapshot(snapshotName))
}

export function expectDisplayResult(code: string, options: TestOptions = {}) {
  return expect(
    testSuccess(code, options)
      .then(snapshot('expectDisplayResult'))
      .then(testResult => testResult.displayResult!)
      .catch(e => console.log(e))
  ).resolves
}

export function expectVisualiseListResult(code: string, options: TestOptions = {}) {
  return expect(
    testSuccess(code, options)
      .then(snapshot('expectVisualiseListResult'))
      .then(testResult => testResult.visualiseListResult)
      .catch(e => console.log(e))
  ).resolves
}

// for use in concurrent testing
export async function getDisplayResult(code: string, options: TestOptions = {}) {
  return await testSuccess(code, options).then(testResult => testResult.displayResult!)
}

export function expectResult(code: string, options: TestOptions = {}) {
  return expect(
    testSuccess(code, options)
      .then(snapshot('expectResult'))
      .then(testResult => testResult.result)
  ).resolves
}

export function expectParsedErrorNoErrorSnapshot(code: string, options: TestOptions = {}) {
  options.showErrorJSON = false
  return expect(
    testFailure(code, options)
      .then(snapshot('expectParsedErrorNoErrorSnapshot'))
      .then(testResult => testResult.parsedErrors)
  ).resolves
}

export function expectParsedError(code: string, options: TestOptions = {}) {
  return expect(
    testFailure(code, options)
      .then(snapshot('expectParsedError'))
      .then(testResult => testResult.parsedErrors)
  ).resolves
}

export function expectDifferentParsedErrors(
  code1: string,
  code2: string,
  options: TestOptions = {}
) {
  return expect(
    testFailure(code1, options).then(error1 => {
      expect(
        testFailure(code2, options).then(error2 => {
          return expect(error1).not.toEqual(error2)
        })
      )
    })
  ).resolves
}

export function expectParsedErrorNoSnapshot(code: string, options: TestOptions = {}) {
  return expect(testFailure(code, options).then(testResult => testResult.parsedErrors)).resolves
}

export function asMockedFunc<T extends (...args: any[]) => any>(func: T) {
  return func as MockedFunction<T>
}

export function expectFinishedResult(result: Result): asserts result is Finished {
  expect(result.status).toEqual('finished')
}

export function expectTrue(cond: boolean): asserts cond {
  expect(cond).toEqual(true)
}

/**
 * Convenience function for testing the expected output of parsing
 * a single line of code
 */
export function astTester<ExpectedError>(
  func: (prog: Program, context: Context, expectedError: ExpectedError | undefined) => void,
  testCases: (
    | [desc: string, code: string]
    | [desc: string, code: string, expectedError: ExpectedError]
  )[],
  chapter: Chapter = Chapter.SOURCE_4
) {
  const fullCases = testCases.map(([desc, code, err]) => {
    const context = mockContext(chapter)
    const program = parse(code, context)
    if (!program) {
      throw context.errors[0]
    }

    return [desc, program, context, err] as [string, Program, Context, ExpectedError | undefined]
  })

  test.each(fullCases)('%s', (_, ...args) => func(...args))
}