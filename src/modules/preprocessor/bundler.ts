import type es from 'estree'

import type { Context } from '../..'
import assert from '../../utils/assert'
import { getModuleDeclarationSource } from '../../utils/ast/helpers'
import { isIdentifier, isImportDeclaration, isModuleDeclaration } from '../../utils/ast/typeGuards'
import { mapAndFilter } from '../../utils/misc'
import type { AbsolutePath } from '../moduleTypes'
import { isSourceModule, resolvePath } from '../utils'
import { createInvokedFunctionResultVariableDeclaration } from './constructors/contextSpecificConstructors'
import {
  transformFilePathToValidFunctionName,
  transformFunctionNameToInvokedFunctionResultVariableName
} from './filePaths'
import hoistAndMergeImports from './transformers/hoistAndMergeImports'
import removeExports from './transformers/removeExports'
import {
  createAccessImportStatements,
  getInvokedFunctionResultVariableNameToImportSpecifiersMap,
  transformProgramToFunctionDeclaration
} from './transformers/transformProgramToFunctionDeclaration'

const getSourceModuleImports = (programs: Record<string, es.Program>): es.ImportDeclaration[] => {
  return Object.values(programs).flatMap(program => {
    return program.body.filter((stmt): stmt is es.ImportDeclaration => {
      if (!isImportDeclaration(stmt)) return false

      const importSource = getModuleDeclarationSource(stmt)
      return isSourceModule(importSource)
    })
  })
}

/**
 * A function that when, given the topological ordering of some programs,
 * bundles them into a single program
 */
export type Bundler = (
  programs: Record<AbsolutePath, es.Program>,
  entrypointFilePath: AbsolutePath,
  topoOrder: AbsolutePath[],
  context: Context
) => es.Program

const defaultBundler: Bundler = (programs, entrypointFilePath, topoOrder) => {
  // We want to operate on the entrypoint program to get the eventual
  // preprocessed program.
  const entrypointProgram = programs[entrypointFilePath]
  const entrypointDirPath = resolvePath(entrypointFilePath, '..')

  // Create variables to hold the imported statements.
  const entrypointProgramModuleDeclarations = entrypointProgram.body.filter(isModuleDeclaration)
  const entrypointProgramInvokedFunctionResultVariableNameToImportSpecifiersMap =
    getInvokedFunctionResultVariableNameToImportSpecifiersMap(
      entrypointProgramModuleDeclarations,
      entrypointDirPath
    )
  const entrypointProgramAccessImportStatements = createAccessImportStatements(
    entrypointProgramInvokedFunctionResultVariableNameToImportSpecifiersMap
  )

  // Transform all programs into their equivalent function declaration
  // except for the entrypoint program.
  const functionDeclarations: Record<string, es.FunctionDeclaration> = {}
  for (const [filePath, program] of Object.entries(programs)) {
    // The entrypoint program does not need to be transformed into its
    // function declaration equivalent as its enclosing environment is
    // simply the overall program's (constructed program's) environment.
    if (filePath === entrypointFilePath) {
      continue
    }

    const functionDeclaration = transformProgramToFunctionDeclaration(program, filePath)
    const functionName = functionDeclaration.id?.name
    assert(
      functionName !== undefined,
      'A transformed function declaration is missing its name. This should never happen.'
    )

    functionDeclarations[functionName] = functionDeclaration
  }

  // Invoke each of the transformed functions and store the result in a variable.
  const invokedFunctionResultVariableDeclarations: es.VariableDeclaration[] = mapAndFilter(
    topoOrder,
    filePath => {
      // As mentioned above, the entrypoint program does not have a function
      // declaration equivalent, so there is no need to process it.
      if (filePath === entrypointFilePath) {
        return undefined
      }

      const functionName = transformFilePathToValidFunctionName(filePath)
      const invokedFunctionResultVariableName =
        transformFunctionNameToInvokedFunctionResultVariableName(functionName)

      const functionDeclaration = functionDeclarations[functionName]
      const functionParams = functionDeclaration.params.filter(isIdentifier)
      assert(
        functionParams.length === functionDeclaration.params.length,
        'Function declaration contains non-Identifier AST nodes as params. This should never happen.'
      )

      const invokedFunctionResultVariableDeclaration =
        createInvokedFunctionResultVariableDeclaration(
          functionName,
          invokedFunctionResultVariableName,
          functionParams
        )
      return invokedFunctionResultVariableDeclaration
    }
  )

  // Get all Source module imports across the entrypoint program & all imported programs.
  const sourceModuleImports = getSourceModuleImports(programs)

  // Re-assemble the program.
  const preprocessedProgram: es.Program = {
    ...entrypointProgram,
    body: [
      ...sourceModuleImports,
      ...Object.values(functionDeclarations),
      ...invokedFunctionResultVariableDeclarations,
      ...entrypointProgramAccessImportStatements,
      ...entrypointProgram.body
    ]
  }

  // We need to hoist all remaining imports to the top of the
  // program. These imports should be source module imports since
  // non-Source module imports would have already been handled. As part
  // of this step, we also merge imports from the same module so as to
  // import each unique name per module only once.
  hoistAndMergeImports(preprocessedProgram)

  // After this pre-processing step, all export-related nodes in the AST
  // are no longer needed and are thus removed.
  removeExports(preprocessedProgram)

  return preprocessedProgram
}

export default defaultBundler