import * as es from 'estree'

import * as create from '../utils/ast/astCreator'
import { getIdentifiersInProgram } from '../utils/uniqueIds'
import { getNativeTranspiler, transpileFilesToSource } from '../transpiler/transpileBundler'
import { transpileFileToSource, type FileTranspiler } from '../transpiler/sourceTranspiler'
import type { Bundler } from '../modules/preprocessor/bundler'
import GPUTransformer from './transfomer'

const gpuFileTranspiler: FileTranspiler = (program, context, nativeId, isEntrypoint) => {
  const transformer = new GPUTransformer(program, create.identifier('__createKernelSource'))
  const res = transformer.transform()

  const gpuDisplayStatements = []
  // add some display statements to program
  if (res.length > 0) {
    for (const arr of res) {
      let debug = `Attempting to optimize ${arr[1]} levels of nested loops starting on line ${arr[0]}`
      if (arr[1] === 1) {
        debug = `Attempting to optimize the loop on line ${arr[0]}`
      }
      gpuDisplayStatements.push(
        create.expressionStatement(
          create.callExpression(create.identifier('display'), [create.literal(debug)], {
            start: { line: 0, column: 0 },
            end: { line: 0, column: 0 }
          })
        )
      )
    }
  }

  const clearKernelCacheStatement = create.expressionStatement(
    create.callExpression(create.identifier('__clearKernelCache'), [], {
      start: { line: 0, column: 0 },
      end: { line: 0, column: 0 }
    })
  )

  const newProgram = create.program([
    ...gpuDisplayStatements,
    clearKernelCacheStatement,
    ...program.body
  ])
  return transpileFileToSource(newProgram, context, nativeId, isEntrypoint)
}

const gpuFilesTranspiler = getNativeTranspiler(gpuFileTranspiler)

// top-level gpu functions that call our code
export const transpileFilesToGPU: Bundler = (linkerSuccess, context) => {
  const isManual = Object.values(linkerSuccess.programs).find(program => {
    const identifiers = getIdentifiersInProgram(program)
    return identifiers.has('__createKernelSource') || identifiers.has('__clearKernelCache')
  })

  if (isManual) {
    const outputProgram = transpileFilesToSource(linkerSuccess, context)
    if (!outputProgram) return undefined

    outputProgram.body.unshift(
      create.expressionStatement(
        create.callExpression(
          create.identifier('display'),
          [
            create.literal(
              'Manual use of GPU library symbols detected, turning off automatic GPU optimizations.'
            )
          ],
          {
            start: { line: 0, column: 0 },
            end: { line: 0, column: 0 }
          }
        )
      )
    )

    return outputProgram
  }

  return gpuFilesTranspiler(linkerSuccess, context)
}

// transpiles if possible and modifies program to a Source program that makes use of the GPU primitives
export function transpileToGPU(program: es.Program) {
  const identifiers = getIdentifiersInProgram(program)
  if (identifiers.has('__createKernelSource') || identifiers.has('__clearKernelCache')) {
    program.body.unshift(
      create.expressionStatement(
        create.callExpression(
          create.identifier('display'),
          [
            create.literal(
              'Manual use of GPU library symbols detected, turning off automatic GPU optimizations.'
            )
          ],
          {
            start: { line: 0, column: 0 },
            end: { line: 0, column: 0 }
          }
        )
      )
    )
    return
  }

  const transformer = new GPUTransformer(program, create.identifier('__createKernelSource'))
  const res = transformer.transform()

  const gpuDisplayStatements = []
  // add some display statements to program
  if (res.length > 0) {
    for (const arr of res) {
      let debug = `Attempting to optimize ${arr[1]} levels of nested loops starting on line ${arr[0]}`
      if (arr[1] === 1) {
        debug = `Attempting to optimize the loop on line ${arr[0]}`
      }
      gpuDisplayStatements.push(
        create.expressionStatement(
          create.callExpression(create.identifier('display'), [create.literal(debug)], {
            start: { line: 0, column: 0 },
            end: { line: 0, column: 0 }
          })
        )
      )
    }
  }

  const clearKernelCacheStatement = create.expressionStatement(
    create.callExpression(create.identifier('__clearKernelCache'), [], {
      start: { line: 0, column: 0 },
      end: { line: 0, column: 0 }
    })
  )

  program.body = [...gpuDisplayStatements, clearKernelCacheStatement, ...program.body]
}
