import type { Program } from 'estree'
import type { Context } from '..'
import { Chapter, Variant } from '../types'
import { transpileFilesToGPU } from '../gpu/gpu'
import { transpileFilesToLazy } from '../lazy/lazy'
import {
  transpileFilesToFullJS,
  transpileFilesToSource,
  type NativeBundler
} from './transpileBundler'

export function transpileSingleFile(program: Program, context: Context, skipUndefined: boolean) {
  let transpiler: NativeBundler
  if (context.variant === Variant.GPU) {
    transpiler = transpileFilesToGPU
  } else if (context.variant === Variant.LAZY) {
    transpiler = transpileFilesToLazy
  } else if (context.chapter === Chapter.FULL_JS || context.chapter === Chapter.PYTHON_1) {
    transpiler = transpileFilesToFullJS
  } else if (context.variant == Variant.NATIVE) {
    transpiler = transpileFilesToFullJS
  } else {
    transpiler = transpileFilesToSource
  }

  return transpiler(
    {
      programs: { '/default.js': program },
      entrypointFilePath: '/default.js',
      topoOrder: []
    },
    context,
    skipUndefined
  )
}
