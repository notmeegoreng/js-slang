import { Result } from "..";
import { RuntimeSourceError } from "../errors/runtimeSourceError";
import { toSourceError } from "./errors";
import { NativeRunner, NativeRunnerArgs } from "./runner";
import { resolvedErrorPromise } from "./utils";

export default class FullJSRunner extends NativeRunner {
  protected async runNative({
    evaler,initObj, transpiledCode, sourceMapJson
  }: NativeRunnerArgs): Promise<Result> {
    try {
      const value = evaler(initObj, transpiledCode)
      
      return {
        status: 'finished',
        value,
        context: this.context,
      }
    } catch (error) {
      this.context.errors.push(
        error instanceof RuntimeSourceError ? error : await toSourceError(error, sourceMapJson)
      )
      return resolvedErrorPromise
    }   
  }
}