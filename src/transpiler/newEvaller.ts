import { NativeStorage } from "../types"

export type Evaler = (obj: InitObj, code: string) => any

export type InitObj = {
  modules: Record<string, any>
  native: NativeStorage
}

export default function getEvalContainer(objId: string, codeId: string) {
  return new Function(
    objId,
    codeId,
    `
    if (${objId}.native.evaller === null) {
      return eval(${codeId})
    } else {
      return ${objId}.native.evaller(${objId}, ${codeId})
    }
    `
  ) as Evaler
}
