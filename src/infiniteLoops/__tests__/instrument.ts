import { mockContext } from '../../mocks/context'
import { parse } from '../../parser/parser'
import { Chapter } from '../../types'
import { evaluateBinaryExpression, evaluateUnaryExpression } from '../../utils/operators'
import {
  InfiniteLoopRuntimeFunctions as functionNames,
  transpileFilesToInfiniteLoop
} from '../instrument'
import { singleFileHelper } from '../../utils/misc'
import { NATIVE_STORAGE_ID } from '../../constants'
import { generate } from 'astring'

function mockFunctionsAndState() {
  const theState = undefined
  const returnFirst = (...args: any[]) => args[0]
  const nothing = (..._args: any[]) => {}

  const functions = {
    [functionNames.nothingFunction]: nothing,
    [functionNames.concretize]: returnFirst,
    [functionNames.hybridize]: returnFirst,
    [functionNames.wrapArg]: returnFirst,
    [functionNames.dummify]: returnFirst,
    [functionNames.saveBool]: returnFirst,
    [functionNames.saveVar]: returnFirst,
    [functionNames.preFunction]: nothing,
    [functionNames.returnFunction]: returnFirst,
    [functionNames.postLoop]: (_: any, res?: any) => res,
    [functionNames.enterLoop]: nothing,
    [functionNames.exitLoop]: nothing,
    [functionNames.trackLoc]: (_1: any, _2: any, fn?: any) => (fn ? fn() : undefined),
    [functionNames.evalB]: evaluateBinaryExpression,
    [functionNames.evalU]: evaluateUnaryExpression
  }
  return [functions, theState]
}

/**
 * Returns the value saved in the code using the builtin 'output'.
 * e.g. runWithMock('output(2)') --> 2
 */
function runWithMock(main: string, codeHistory?: string[], builtins: Map<string, any> = new Map()) {
  let output = undefined
  builtins.set('output', (x: any) => (output = x))
  builtins.set('undefined', undefined)
  const context = mockContext(Chapter.SOURCE_4)
  context.nativeStorage.evaller = () => 0
  context.nativeStorage.builtins = builtins

  const program = parse(main, context)
  expect(program).toBeTruthy()

  if (codeHistory !== undefined) {
    const restOfCode = codeHistory.map(x => parse(x, context))
    for (const code of restOfCode) {
      expect(code).toBeTruthy()
      context.previousPrograms.push(code!)
    }
  }

  const prelude = parse(context.prelude!, context)!
  expect(prelude).toBeTruthy()
  context.previousPrograms.push(prelude)

  const [mockFunctions, mockState] = mockFunctionsAndState()
  const instrumentedProgram = singleFileHelper(
    transpileFilesToInfiniteLoop,
    program!,
    context,
    true
  )
  expect(instrumentedProgram).toBeTruthy()

  const instrumentedCode = generate(instrumentedProgram)
  const sandboxedRun = new Function(
    'code',
    NATIVE_STORAGE_ID,
    // redeclare window so modules don't do anything funny like play sounds
    '{let window = {}; return eval(code)}'
  )

  try {
    sandboxedRun(instrumentedCode, {
      builtinsId: builtins,
      functionsId: mockFunctions,
      stateId: mockState,
      loadedModules: {}
    })
  } catch (error) {
    console.error(error)
    throw error
  }
  return output
}

type TestCase = [desc: string, mainCode: string, prevCode: string[], expected: any]
const testCases: TestCase[] = [
  ['builtins work', 'output(2);', [], 2],
  ['binary expressions work', 'output(1+1);', [], 2],
  ['unary expressions work', 'output(!true);', [], false],
  [
    'assigment works as expected',
    `let x = 2;
    let a = [];
    a[0] = 3;
    output(x+a[0]);`,
    [],
    5
  ],
  ['globals from old code accessible', 'output(z+1);', ['const z = w+1;', 'let w = 10;'], 12],
  [
    'functions run as expected',
    `function f(x,y) {
      return x===0?x:f(x-1,y)+y;
    }
    output(f(5,2));`,
    [],
    10
  ],
  [
    'nested functions run as expected',
    `function f(x,y) {
      function f(x,y) {
        return 0;
      }
      return x===0?x:f(x-1,y)+y;
    }
    output(f(5,2));`,
    [],
    2
  ],
  [
    'higher order functions run as expected',
    `function run(f, x) {
       return f(x+1);
     }
    output(run(x=>x+1, 1));`,
    [],
    3
  ],
  [
    'loops run as expected',
    `let w = 0;
    for (let i = w; i < 10; i=i+1) {w = i;}
    output(w);`,
    [],
    9
  ],
  [
    'nested loops run as expected',
    `let w = 0;
      for (let i = w; i < 10; i=i+1) {
        for (let j = 0; j < 10; j=j+1) {
          w = w + 1;
        }
      }
      output(w);`,
    [],
    100
  ],
  [
    'multidimensional arrays work',
    `const x = [[1],[2]];
    output(x[1] === undefined? undefined: x[1][0]);`,
    [],
    2
  ],
  [
    'if statements work as expected',
    `let x = 1;
    if (x===1) {
     x = x + 1;
    } else {}
    output(x);`,
    [],
    2
  ],
  [
    'combination of loops and functions run as expected',
    `function test(x) {
       return x===0;
     }
     const minus = (a,b) => a-b;
     let w = 10;
     let z = 0;
     while(!test(w)) {
       for (let j = 0; j < 10; j=j+1) {
         z = z + 1;
       }
       w = minus(w,1);
     }
     output(z);`,
    [],
    100
  ]
]

test.each(testCases)('%s', (_, main, prev, expected) =>
  expect(runWithMock(main, prev)).toEqual(expected)
)
