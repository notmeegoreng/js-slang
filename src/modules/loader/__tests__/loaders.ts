import { mockContext } from '../../../mocks/context'
import { Chapter, Variant } from '../../../types'
import { ModuleConnectionError } from '../../errors'
import * as moduleLoader from '../loaders'

jest.mock('lodash', () => ({
  ...jest.requireActual('lodash'),
  memoize: jest.fn(x => x)
}))

const moduleMocker = jest.fn()

jest.mock(
  `${jest.requireActual('../loaders').MODULES_STATIC_URL}/bundles/one_module.js`,
  () => ({
    default: moduleMocker
  }),
  { virtual: true }
)

jest.mock(
  `${jest.requireActual('../loaders').MODULES_STATIC_URL}/modules.json`,
  () => ({
    default: {
      one_module: { tabs: ['tab1', 'tab2'] }
    }
  }),
  { virtual: true }
)

jest.mock(
  `${jest.requireActual('../loaders').MODULES_STATIC_URL}/tabs/tab1.js`,
  () => ({
    default: () => 'tab1'
  }),
  { virtual: true }
)

jest.mock(
  `${jest.requireActual('../loaders').MODULES_STATIC_URL}/tabs/tab2.js`,
  () => ({
    default: () => 'tab2'
  }),
  { virtual: true }
)

global.fetch = jest.fn()

beforeEach(() => {
  jest.clearAllMocks()
})

afterEach(() => {
  jest.resetModules()
})

describe('bundle loading', () => {
  test('Loading a single bundle', async () => {
    const context = mockContext(Chapter.SOURCE_4, Variant.DEFAULT)
    moduleMocker.mockReturnValueOnce({
      foo() {
        return this.foo.name
      },
      bar: () => 'bar'
    })
    const mod = await moduleLoader.loadModuleBundleAsync('one_module', context)
    expect(mod.foo()).toEqual('foo')
    expect(mod.foo[moduleLoader.sourceModuleObject]).toEqual(true)
  })

  test('Should throw ModuleConnectionError when unable to reach modules server', () => {
    const context = mockContext(Chapter.SOURCE_4, Variant.DEFAULT)
    const promise = moduleLoader.loadModuleBundleAsync('unknown_module', context)
    return expect(promise).rejects.toBeInstanceOf(ModuleConnectionError)
  })
})

describe('tab loading', () => {
  test("Load a module's tabs", async () => {
    const tabs = await moduleLoader.loadModuleTabsAsync('one_module')

    expect(tabs[0]({} as any)).toEqual('tab1')
    expect(tabs[1]({} as any)).toEqual('tab2')
  })
})
