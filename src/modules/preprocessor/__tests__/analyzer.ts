import createContext from '../../../createContext'
import {
  DuplicateImportNameError,
  UndefinedDefaultImportError,
  UndefinedImportError,
  UndefinedNamespaceImportError
} from '../../errors'
import { Chapter } from '../../../types'
import { stripIndent } from '../../../utils/formatters'
import parseProgramsAndConstructImportGraph from '../linker'
import analyzeImportsAndExports from '../analyzer'
import loadSourceModules from '../../loader'
import type { SourceFiles as Files } from '../../moduleTypes'
import { objectKeys } from '../../../utils/misc'
import { testTrueAndFalseCases } from '../../../utils/testing'

jest.mock('../../loader/loaders')

beforeEach(() => {
  jest.clearAllMocks()
})

async function testCode<T extends Files>(
  files: T,
  entrypointFilePath: keyof T,
  allowUndefinedImports: boolean,
  throwOnDuplicateNames: boolean
) {
  const context = createContext(Chapter.FULL_JS)
  const importGraphResult = await parseProgramsAndConstructImportGraph(
    p => Promise.resolve(files[p]),
    entrypointFilePath as string,
    context,
    {},
    true
  )

  // Return 'undefined' if there are errors while parsing.
  if (context.errors.length !== 0 || !importGraphResult.ok) {
    throw context.errors[0]
  }

  const { programs, topoOrder, sourceModulesToImport } = importGraphResult
  await loadSourceModules(sourceModulesToImport, context, false)

  try {
    analyzeImportsAndExports(programs, entrypointFilePath as string, topoOrder, context, {
      allowUndefinedImports,
      throwOnDuplicateNames
    })
  } catch (error) {
    if (
      !(error instanceof DuplicateImportNameError) &&
      !(error instanceof UndefinedNamespaceImportError)
    ) {
      throw error
    }

    return error
  }
  return true
}

describe('Test throwing import validation errors', () => {
  type ErrorInfo = {
    line: number
    col: number
    moduleName: string
  } & (
    | {
        type?: undefined
        /**
         * Set this to a value if you are expecting an undefined import error
         * to be thrown with the given symbol
         */
        symbol: Exclude<string, 'default'>
      }
    | {
        type: 'namespace' | 'default'
      }
  )

  // Providing an ErrorInfo object indicates that the test case should throw
  // the corresponding error
  type ImportTestCaseWithNoError = [string, Files, string]
  type ImportTestCaseWithError = [...ImportTestCaseWithNoError, ErrorInfo]
  type ImportTestCase = ImportTestCaseWithError | ImportTestCaseWithNoError

  function expectValidationError(obj: any): asserts obj is UndefinedNamespaceImportError {
    expect(obj).toBeInstanceOf(UndefinedNamespaceImportError)
  }

  async function testFailure<T extends Files>(
    files: T,
    entrypointFilePath: keyof T,
    allowUndefinedImports: boolean,
    errInfo: ErrorInfo
  ) {
    const err = await testCode(files, entrypointFilePath, allowUndefinedImports, false)

    expectValidationError(err)
    expect(err.moduleName).toEqual(errInfo.moduleName)

    switch (errInfo.type) {
      case 'namespace': {
        // Check namespace import
        expect(err).toBeInstanceOf(UndefinedNamespaceImportError)
        break
      }
      case 'default': {
        expect(err).toBeInstanceOf(UndefinedDefaultImportError)
        break
      }
      default: {
        expect(err).toBeInstanceOf(UndefinedImportError)
        expect((err as UndefinedImportError).symbol).toEqual(errInfo.symbol)
      }
    }

    expect(err.location.start).toMatchObject({
      line: errInfo.line,
      column: errInfo.col
    })
  }

  function testSuccess<T extends Files>(
    files: T,
    entrypointFilePath: keyof T,
    allowUndefinedImports: boolean
  ) {
    return expect(
      testCode(files, entrypointFilePath, allowUndefinedImports, false)
    ).resolves.toEqual(true)
  }

  type FullTestCase = [string, Files, `/${string}`, ErrorInfo | boolean]
  function testCases(desc: string, cases: ImportTestCase[]) {
    testTrueAndFalseCases(
      desc,
      'allowUndefinedImports',
      cases,
      ([desc, files, entry, errorInfo]) => {
        return [
          [`${desc} should not throw an error`, files, entry, true] as FullTestCase,
          [
            `${desc} should${errorInfo ? '' : ' not'} throw an error`,
            files,
            entry,
            errorInfo
          ] as FullTestCase
        ]
      },
      async ([files, entrypointFilePath, errorInfo]) => {
        if (errorInfo === true) {
          // If allowUndefinedImports is true, the analyzer should never throw an error
          await testSuccess(files, entrypointFilePath, true)
        } else if (!errorInfo) {
          // Otherwise it should not throw when no errors are expected
          await testSuccess(files, entrypointFilePath, false)
        } else {
          // Or throw the expected error
          await testFailure(files, entrypointFilePath, false, errorInfo)
        }
      },
      true
    )
  }

  describe('Test regular imports', () => {
    testCases('Local imports', [
      [
        'Regular local import',
        {
          '/a.js': 'export const a = "a";',
          '/b.js': stripIndent`
            import { a } from "./a.js";

            export function b() {
              return a;
            }
          `
        },
        '/b.js'
      ],
      [
        'Regular local import with unknown symbol',
        {
          '/a.js': 'export const a = "a";',
          '/b.js': stripIndent`
            import { a, unknown } from "./a.js";

            export function b() {
              return a;
            }
          `
        },
        '/b.js',
        { moduleName: '/a.js', line: 1, col: 12, symbol: 'unknown' }
      ],
      [
        'Regular local import of exported function declaration',
        {
          '/a.js': `export function a() { return 0; }`,
          '/b.js': `import { a } from './a.js';`
        },
        '/b.js'
      ]
    ])

    testCases('Source imports', [
      [
        'Regular Source import',
        {
          '/a.js': stripIndent`
            import { foo, bar } from "one_module";
            export function b() {
              return foo();
            }
          `
        },
        '/a.js'
      ],
      [
        'Regular Source import with unknown symbol',
        {
          '/a.js': stripIndent`
            import { foo, unknown } from "one_module";
            export function b() {
              return foo();
            }
          `
        },
        '/a.js',
        { line: 1, col: 14, moduleName: 'one_module', symbol: 'unknown' }
      ]
    ])

    testCases('Source and Local imports', [
      [
        'Regular Local and Source imports',
        {
          '/a.js': 'export const a = "a";',
          '/b.js': stripIndent`
            import { a } from "./a.js";
            import { bar } from 'one_module';

            export function b() {
              bar();
              return a;
            }
          `
        },
        '/b.js'
      ],
      [
        'Regular Local and Source imports with unknown symbol',
        {
          '/a.js': 'export const a = "a";',
          '/b.js': stripIndent`
            import { a } from "./a.js";
            import { unknown } from 'one_module';

            export function b() {
              unknown();
              return a;
            }
          `
        },
        '/b.js',
        { moduleName: 'one_module', line: 2, col: 9, symbol: 'unknown' }
      ],
      [
        'Regular Local and Source imports with unknown symbol',
        {
          '/a.js': 'export const a = "a";',
          '/b.js': stripIndent`
            import { a, unknown } from "./a.js";
            import { foo } from 'one_module';

            export function b() {
              foo();
              return a;
            }
          `
        },
        '/b.js',
        { moduleName: '/a.js', line: 1, col: 12, symbol: 'unknown' }
      ]
    ])
  })

  describe('Test default imports', () => {
    testCases('Local imports', [
      [
        'Default import from local module',
        {
          '/a.js': 'const a = "a"; export default a;',
          '/b.js': stripIndent`
            import a from "./a.js";

            export function b() {
              return a;
            }
          `
        },
        '/b.js'
      ],
      [
        'Default import from local module with no default export',
        {
          '/a.js': 'export const a = "a";',
          '/b.js': stripIndent`
            import unknown, { a } from "./a.js";

            export function b() {
              return a;
            }
          `
        },
        '/b.js',
        { moduleName: '/a.js', line: 1, col: 7, type: 'default' }
      ],
      [
        'Default import from local module with no default export',
        {
          '/a.js': 'export const a = "a";',
          '/b.js': stripIndent`
            import unknown from "./a.js";

            export function b() {
              return a;
            }
          `
        },
        '/b.js',
        { moduleName: '/a.js', line: 1, col: 7, type: 'default' }
      ],
      [
        'Default import using regular specifier from local module with no default export',
        {
          '/a.js': 'export const a = "a";',
          '/b.js': stripIndent`
            import { default as unknown } from "./a.js";

            export function b() {
              return a;
            }
          `
        },
        '/b.js',
        { moduleName: '/a.js', line: 1, col: 9, type: 'default' }
      ],
      [
        'Default import with function as default export',
        {
          '/a.js': 'export default function a() { return 0; }',
          '/b.js': "import a from './a.js';"
        },
        '/b.js'
      ]
    ])

    testCases('Source imports', [
      [
        'Default import from Source module without default export',
        {
          '/a.js': stripIndent`
            import foo from "another_module";
            export function b() {
              return foo();
            }
          `
        },
        '/a.js',
        { moduleName: 'another_module', line: 1, col: 7, type: 'default' }
      ],
      [
        'Default import using regular specifier from Source module without default export',
        {
          '/a.js': stripIndent`
            import { default as foo } from "another_module";
            export function b() {
              return foo();
            }
          `
        },
        '/a.js',
        { moduleName: 'another_module', line: 1, col: 9, type: 'default' }
      ]
    ])

    testCases('Source and Local imports', [
      [
        'Default imports',
        {
          '/a.js': 'const a = "a"; export default a',
          '/b.js': stripIndent`
            import a from "./a.js";
            import { bar } from 'one_module';

            export function b() {
              bar();
              return a;
            }
          `
        },
        '/b.js'
      ],
      [
        'Default imports',
        {
          '/a.js': 'export const a = "a";',
          '/b.js': stripIndent`
            import { a } from "./a.js";
            import unknown from 'another_module';

            export function b() {
              unknown();
              return a;
            }
          `
        },
        '/b.js',
        { moduleName: 'another_module', line: 2, col: 7, type: 'default' }
      ],
      [
        'Default imports',
        {
          '/a.js': 'export const a = "a";',
          '/b.js': stripIndent`
            import unknown, { a } from "./a.js";
            import { default as foo } from 'another_module';

            export function b() {
              foo();
              return a;
            }
          `
        },
        '/b.js',
        { moduleName: '/a.js', line: 1, col: 7, type: 'default' }
      ]
    ])
  })

  describe('Test namespace imports', () => {
    testCases('Local imports', [
      [
        'Regular namespace import',
        {
          '/a.js': 'export const a = 0;',
          '/b.js': 'import * as a from "./a.js"'
        },
        '/b.js'
      ],
      [
        'Regular namespace import from local module that does not export anything',
        {
          '/a.js': 'const a = 0;',
          '/b.js': 'import * as a from "./a.js"'
        },
        '/b.js',
        { line: 1, col: 7, moduleName: '/a.js', type: 'namespace' }
      ]
    ])

    testCases('Source imports', [
      [
        'Regular namespace import',
        {
          '/a.js': 'import * as bar from "one_module";'
        },
        '/a.js'
      ]
    ])
  })

  describe('Test named exports', () => {
    testCases('Exporting from another local module', [
      [
        'Regular named reexport',
        {
          '/a.js': 'export const a = 0;',
          '/b.js': 'export { a } from "./a.js"'
        },
        '/b.js'
      ],
      [
        'Regular named reexport of undefined symbol',
        {
          '/a.js': 'export const a = 0;',
          '/b.js': 'export { b } from "./a.js"'
        },
        '/b.js',
        { line: 1, col: 9, moduleName: '/a.js', symbol: 'b' }
      ],
      [
        'Regular named reexport of undefined symbol with alias',
        {
          '/a.js': 'export const a = 0;',
          '/b.js': 'export { b as a } from "./a.js"'
        },
        '/b.js',
        { line: 1, col: 9, moduleName: '/a.js', symbol: 'b' }
      ],
      [
        'Regular named reexport of undefined symbol using ExportAllDeclaration',
        {
          '/a.js': 'export const a = "a"',
          '/b.js': 'export * from "./a.js"',
          '/c.js': 'export { a } from "./b.js"'
        },
        '/c.js'
      ],
      [
        'Regular named reexport of unknown default export with alias',
        {
          '/a.js': 'export const a = "a";',
          '/b.js': 'export { default as b } from "./a.js"'
        },
        '/b.js',
        { line: 1, col: 9, moduleName: '/a.js', type: 'default' }
      ]
    ])
  })

  describe('Test export all declarations', () => {
    testCases('Exporting from another local module', [
      [
        'Regular ExportAllDeclaration',
        {
          '/a.js': 'export const a = "a"',
          '/b.js': 'export * from "./a.js"'
        },
        '/a.js'
      ],
      [
        'Regular ExportAllDeclaration',
        {
          '/a.js': 'const a = "a"',
          '/b.js': 'export * from "./a.js"'
        },
        '/b.js',
        { line: 1, col: 0, moduleName: '/a.js', type: 'namespace' }
      ]
    ])
  })

  testCases('Test transitivity', [
    // ExportAllDeclarations
    [
      'Regular ExportAllDeclaration 1',
      {
        '/a.js': 'export const a = 0;',
        '/b.js': 'export * from "./a.js";',
        '/c.js': 'import { a } from "./b.js";'
      },
      '/c.js'
    ],
    [
      'Regular ExportAllDeclaration 2',
      {
        '/a.js': 'const a = 0;',
        '/b.js': 'export * from "./a.js";',
        '/c.js': 'import { a } from "./b.js"'
      },
      '/c.js',
      { line: 1, col: 0, moduleName: '/a.js', type: 'namespace' }
    ],
    [
      'ExportAllDeclarations should not reexport default exports 1',
      {
        '/a.js': 'export default function a() {}',
        '/b.js': 'export * from "./a.js";',
        '/c.js': 'import a from "./b.js";'
      },
      '/c.js',
      { line: 1, col: 7, moduleName: '/b.js', type: 'namespace' }
    ],
    [
      'ExportAllDeclarations should not reexport default exports 2',
      {
        '/a.js': `
          export default function a() {}
          export const b = 0;
        `,
        '/b.js': 'export * from "./a.js";',
        '/c.js': 'import a from "./b.js";'
      },
      '/c.js',
      { line: 1, col: 7, moduleName: '/b.js', type: 'default' }
    ],
    [
      'Default exports should not be shadowed by ExportAllDeclarations',
      {
        '/a.js': `
          const a = 0;
          export default a;
          export const b = 0;
        `,
        '/b.js': `
          export * from './a.js';
          export default function b() {}
        `,
        '/c.js': 'import a from "./b.js";'
      },
      '/c.js'
    ],

    // ExportNamedDeclarations
    [
      'ExportNamedDeclarations can reexport default exports',
      {
        '/a.js': 'export default function a() {}',
        '/b.js': 'export { default } from "./a.js";',
        '/c.js': 'import a from "./b.js";'
      },
      '/c.js'
    ]
  ])
})

describe('Test throwing DuplicateImportNameErrors', () => {
  /**
   * [Description, Files]
   * Use this test case specification to specify that no error is expected
   */
  type TestCaseWithNoError<T extends Files> = [description: string, files: T]

  /**
   * [Description, Files, Expected location string]
   * Use this test case specification to specify that an error is expected.
   * The given string represents the location string
   */
  type TestCaseWithError<T extends Files> = [description: string, files: T, expectedError: string]

  type TestCase<T extends Files> = TestCaseWithError<T> | TestCaseWithNoError<T>
  const isTestCaseWithNoError = <T extends Files>(c: TestCase<T>): c is TestCaseWithNoError<T> =>
    c.length === 2

  type FullTestCase = [Files, true, string | undefined] | [Files, false, undefined]

  function expectDuplicateError(obj: any): asserts obj is DuplicateImportNameError {
    expect(obj).toBeInstanceOf(DuplicateImportNameError)
  }

  function testCases<T extends Files>(desc: string, cases: TestCase<T>[]) {
    testTrueAndFalseCases<TestCase<any>, FullTestCase>(
      desc,
      'throwOnDuplicateImports',
      cases,
      c => {
        // For each test case, split it into the case where throwOnDuplicateImports is true
        // and when it is false. No errors should ever be thrown when throwOnDuplicateImports is false
        if (isTestCaseWithNoError(c)) {
          // No error message was given, so no error is expected to be thrown,
          // regardless of the value of throwOnDuplicateImports
          const [desc] = c
          const noThrowCase: [string, ...FullTestCase] = [
            `${desc}: no error `,
            c[1],
            false,
            undefined
          ]
          const yesThrowCase: [string, ...FullTestCase] = [
            `${desc}: no error`,
            c[1],
            true,
            undefined
          ]
          return [noThrowCase, yesThrowCase]
        }

        const [desc, , errMsg] = c
        const noThrowCase: [string, ...FullTestCase] = [`${desc}: no error`, c[1], false, undefined]
        const yesThrowCase: [string, ...FullTestCase] = [`${desc}: error`, c[1], true, errMsg]
        return [noThrowCase, yesThrowCase]
      },
      async ([files, shouldThrow, errMsg]) => {
        const [entrypointFilePath] = objectKeys(files)

        const promise = testCode(files, entrypointFilePath, true, shouldThrow)
        if (!shouldThrow || errMsg === undefined) {
          return expect(promise).resolves.toEqual(true)
        }

        const err = await promise
        expectDuplicateError(err)

        // Make sure the locations are always displayed in order
        // for consistency across tests (ok since locString should be order agnostic)
        const segments = err.locString.split(',').map(each => each.trim())
        segments.sort()

        expect(segments.join(', ')).toEqual(errMsg)
      },
      true
    )
  }

  testCases('Imports from different modules', [
    [
      'Different imports from different Source modules across multiple files',
      {
        '/a.js': `import { foo as a } from 'one_module';
        import { b } from './b.js';
        `,
        '/b.js': `import { bar as a } from 'another_module';`
      },
      '(/a.js:1:9), (/b.js:1:9)'
    ],
    [
      'Different imports from different local modules across multiple files',
      {
        '/a.js': 'import { foo as a } from "./b.js";',
        '/b.js': 'import { bar as a } from "./c.js";',
        '/c.js': 'export function bar() {}'
      }
    ],
    [
      'Different imports including default imports across multiple files',
      {
        '/a.js': `import a from 'one_module';
        import { b } from './b.js';
        `,
        '/b.js': `import a from 'another_module';`
      },
      '(/a.js:1:7), (/b.js:1:7)'
    ],
    [
      'Different imports of different types from Source modules',
      {
        '/a.js': `import { foo as a } from 'one_module';
        import { b } from './b.js';
        `,
        '/b.js': `import a from 'another_module';`
      },
      '(/a.js:1:9), (/b.js:1:7)'
    ],
    [
      'Different imports from both Source and local modules',
      {
        '/a.js': `import { foo as a } from 'one_module';
        import { b } from './b.js';
        `,
        '/b.js': `import { c } from './c.js';`,
        '/c.js': 'export function c() {}'
      }
    ],
    [
      'Namespace imports from Source modules',
      {
        '/a.js': `import * as a from 'one_module';
        import { b } from './b.js';
        `,
        '/b.js': `import * as a from 'another_module';`
      },
      '(/a.js:1:7), (/b.js:1:7)'
    ],
    [
      'Three conflicting imports',
      {
        '/a.js': `import * as a from 'one_module';
        import { b } from './b.js';
        import { c } from './c.js';
        `,
        '/b.js': `import a from 'another_module';`,
        '/c.js': `import { foo as a } from 'one_module';`
      },
      '(/a.js:1:7), (/b.js:1:7), (/c.js:1:9)'
    ]
  ])

  testCases('Imports from the same Source module', [
    [
      'Same import across multiple files 1',
      {
        '/a.js': `import { foo as a } from 'one_module';
        import { b } from './b.js';
        `,
        '/b.js': `import { foo as a } from 'one_module';`
      }
    ],
    [
      'Same import across multiple files 2',
      {
        '/a.js': `import { foo as a } from 'one_module';
        
        import { b } from './b.js';
        import { c } from './c.js';
        import { d } from './d.js';
        `,
        '/b.js': `import { foo as a } from 'one_module';`,
        '/c.js': `import { foo as b } from 'one_module';`,
        '/d.js': `import { foo as b } from 'one_module';`
      }
    ],
    [
      'Different import across multiple files',
      {
        '/a.js': `import { foo as a } from 'one_module';
        import { b } from './b.js';
        `,
        '/b.js': `import { bar as a } from 'one_module';`
      },
      '(/a.js:1:9), (/b.js:1:9)'
    ],
    [
      'Different namespace imports across multiple files',
      {
        '/a.js': `import * as a from 'one_module';
        import { b } from './b.js';
        `,
        '/b.js': `import * as a from 'one_module';`
      }
    ],
    [
      'Same default import across multiple files',
      {
        '/a.js': `import a from 'one_module';
        import { b } from './b.js';
        `,
        '/b.js': `import a from 'one_module';`
      }
    ],
    [
      'Different types of imports across multiple files 1',
      {
        '/a.js': `import { foo as a } from 'one_module';
        import { b } from './b.js';
        `,
        '/b.js': `import a from 'one_module';`
      },
      '(/a.js:1:9), (/b.js:1:7)'
    ],
    [
      'Different types of imports across multiple files 2',
      {
        '/a.js': `import * as a from 'one_module';
        import { b } from './b.js';
        `,
        '/b.js': `import a from 'one_module';`
      },
      '(/a.js:1:7), (/b.js:1:7)'
    ],
    [
      'Different types of imports across multiple files 3',
      {
        '/a.js': `import * as a from 'one_module';
        import { b } from './b.js';
        import { c } from './c.js';
        `,
        '/b.js': `import a from 'one_module';`,
        '/c.js': `import * as a from 'one_module';`
      },
      '(/a.js:1:7), (/b.js:1:7), (/c.js:1:7)'
    ],
    [
      'Different types of imports across multiple files 4',
      {
        '/a.js': `import * as a from 'one_module';
        import { b } from './b.js';
        import { c } from './c.js';
        `,
        '/b.js': `import a from 'one_module';`,
        '/c.js': `import { foo as a } from 'one_module';`
      },
      '(/a.js:1:7), (/b.js:1:7), (/c.js:1:9)'
    ],
    [
      'Handles aliasing correctly 1',
      {
        '/a.js': `import a from 'one_module';
        import { b } from './b.js';
        `,
        '/b.js': `import { default as a } from 'one_module';`
      }
    ],
    [
      'Handles aliasing correctly 2',
      {
        '/a.js': `import { foo as a } from 'one_module';
        import { b } from './b.js';
        `,
        '/b.js': `import { foo } from 'one_module';`
      }
    ],
    [
      'Handles aliasing correctly 3',
      {
        '/a.js': `import { foo as a } from 'one_module';
        import { b } from './b.js';
        `,
        '/b.js': `import { a as foo } from 'one_module';`
      }
    ]
  ])
})
