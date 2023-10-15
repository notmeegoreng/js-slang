import { Program } from "estree";

import { Result } from "..";
import { ECEResultPromise, evaluate as ECEvaluate } from '../ec-evaluator/interpreter'
import { callee, getEvaluationSteps, getRedex, IStepperPropContents, redexify } from "../stepper/stepper";
import { PreprocessRunner } from "./runner";
import { resolvedErrorPromise } from "./utils";

export class ECRunner extends PreprocessRunner {
  protected runPreprocess(program: Program): Promise<Result> {
    const value = ECEvaluate(program, this.context, this.options)
    return ECEResultPromise(this.context, value)
  }
}

export class SubstRunner extends PreprocessRunner {
  protected async runPreprocess(program: Program): Promise<Result> {
    const steps = await getEvaluationSteps(program, this.context, this.options)
    if (this.context.errors.length > 0) {
      return resolvedErrorPromise
    }
    const redexedSteps: IStepperPropContents[] = []
    for (const step of steps) {
      const redex = getRedex(step[0], step[1])
      const redexed = redexify(step[0], step[1])
      redexedSteps.push({
        code: redexed[0],
        redex: redexed[1],
        explanation: step[2],
        function: callee(redex, this.context)
      })
    }
    return {
      status: 'finished',
      context: this.context,
      value: redexedSteps
    }
  }
}