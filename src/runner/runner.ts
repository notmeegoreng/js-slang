import { generate } from "astring";
import type * as es from "estree";
import { posix as posixPath } from 'path';
import { RawSourceMap, SourceMapGenerator } from "source-map";

import { Context, IOptions, Result } from "..";
import { createInvokedFunctionResultVariableDeclaration } from "../localImports/constructors/contextSpecificConstructors";
import { transformFilePathToValidFunctionName, transformFunctionNameToInvokedFunctionResultVariableName } from "../localImports/filePaths";
import { hoistAndMergeImports } from "../localImports/transformers/hoistAndMergeImports";
import { removeExports } from "../localImports/transformers/removeExports";
import { removeNonSourceModuleImports } from "../localImports/transformers/removeNonSourceModuleImports";
import { createAccessImportStatements, getInvokedFunctionResultVariableNameToImportSpecifiersMap, transformProgramToFunctionDeclaration } from "../localImports/transformers/transformProgramToFunctionDeclaration";
import { initModuleContextAsync, loadModuleBundleAsync } from "../modules/moduleLoaderAsync";
import { ModuleFunctions } from "../modules/moduleTypes";
import transpilePrograms, { ProgramTranspiler } from "../transpiler/baseTranspiler";
import getEvalContainer, { Evaler, InitObj } from "../transpiler/newEvaller";
import assert from "../utils/assert";
import { isImportDeclaration, isModuleDeclaration, isSourceModule } from "../utils/ast/typeGuards";
import { isIdentifier } from "../utils/rttc";

export default abstract class Runner<TResult = Result> {
  public readonly entrypointFilePath: string;

  constructor(
    public readonly programs: Record<string, es.Program>,
    public readonly topoOrder: string[],
    public readonly context: Context,
    public readonly options: IOptions,
  ) {
    assert(topoOrder.length >= 1, 'Topological ordering should contain at least the entrypoint file!')
    this.entrypointFilePath = topoOrder[topoOrder.length - 1];
  }

  public abstract run(): Promise<TResult>
}

export abstract class PreprocessRunner<TResult = Result> extends Runner<TResult> {
  private static getSourceModuleImports = (programs: Record<string, es.Program>): es.ImportDeclaration[] => {
    const sourceModuleImports: es.ImportDeclaration[] = []
    Object.values(programs).forEach((program: es.Program): void => {
      const importDeclarations = program.body.filter(isImportDeclaration)
      importDeclarations.forEach((importDeclaration): void => {
        const importSource = importDeclaration.source.value
        if (typeof importSource !== 'string') {
          throw new Error('Module names must be strings.')
        }
        if (isSourceModule(importSource)) {
          sourceModuleImports.push(importDeclaration)
        }
      })
    })
    return sourceModuleImports
  }

  public override run(): Promise<TResult> {
    // We want to operate on the entrypoint program to get the eventual
    // preprocessed program.
    const entrypointProgram = this.programs[this.entrypointFilePath]
    const entrypointDirPath = posixPath.resolve(this.entrypointFilePath, '..')

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
    for (const [filePath, program] of Object.entries(this.programs)) {
      // The entrypoint program does not need to be transformed into its
      // function declaration equivalent as its enclosing environment is
      // simply the overall program's (constructed program's) environment.
      if (filePath === this.entrypointFilePath) {
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
    const invokedFunctionResultVariableDeclarations: es.VariableDeclaration[] = []
    this.topoOrder.forEach((filePath: string): void => {
      // As mentioned above, the entrypoint program does not have a function
      // declaration equivalent, so there is no need to process it.
      if (filePath === this.entrypointFilePath) {
        return
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

      const invokedFunctionResultVariableDeclaration = createInvokedFunctionResultVariableDeclaration(
        functionName,
        invokedFunctionResultVariableName,
        functionParams
      )
      invokedFunctionResultVariableDeclarations.push(invokedFunctionResultVariableDeclaration)
    })

    // Get all Source module imports across the entrypoint program & all imported programs.
    const sourceModuleImports = PreprocessRunner.getSourceModuleImports(this.programs)

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

    // After this pre-processing step, all export-related nodes in the AST
    // are no longer needed and are thus removed.
    removeExports(preprocessedProgram)
    // Likewise, all import-related nodes in the AST which are not Source
    // module imports are no longer needed and are also removed.
    removeNonSourceModuleImports(preprocessedProgram)
    // Finally, we need to hoist all remaining imports to the top of the
    // program. These imports should be source module imports since
    // non-Source module imports would have already been removed. As part
    // of this step, we also merge imports from the same module so as to
    // import each unique name per module only once.
    hoistAndMergeImports(preprocessedProgram)

    return this.runPreprocess(preprocessedProgram)
  }

  protected abstract runPreprocess(program: es.Program): Promise<TResult>
}

export type NativeRunnerArgs = {
  transpiledCode: string
  initObj: InitObj
  evaler: Evaler
  sourceMapJson: RawSourceMap
}

export abstract class NativeRunner<TResult = Result> extends Runner<TResult> {
  constructor(
    programs: Record<string, es.Program>,
    topoOrder: string[],
    context: Context,
    options: IOptions,
    protected readonly getTranspiler: (...args: ConstructorParameters<typeof ProgramTranspiler>) => ProgramTranspiler
  ) {
    super(programs, topoOrder, context, options)
  }

  protected abstract runNative(args: NativeRunnerArgs): Promise<TResult>;

  public override async run(): Promise<TResult> {
    const sourceMap = new SourceMapGenerator()
    const {
      newProgram,
      codeId,
      globalObjId,
      sourceModulesToImport
    } = await transpilePrograms(
      this.programs,
      this.topoOrder,
      this.context,
      this.getTranspiler,
      new Set(),
    )

    const loadedSourceModules = await Promise.all([...sourceModulesToImport.keys()].map(
      async moduleName => {
        const [bundle] = await Promise.all([
          loadModuleBundleAsync(moduleName, this.context, this.options.importOptions.wrapSourceModules),
          initModuleContextAsync(moduleName, this.context, this.options.importOptions.loadTabs)
        ])

        return [moduleName, bundle] as [string, ModuleFunctions]
      }))
    const transpiledCode = generate(newProgram, { sourceMap })
    const evaler = getEvalContainer(globalObjId, codeId)

    return this.runNative(
      {
        transpiledCode,
        initObj: {
          modules: Object.fromEntries(loadedSourceModules),
          native: this.context.nativeStorage
        },
        evaler,
        sourceMapJson: sourceMap.toJSON()
      }
    )
  }
}