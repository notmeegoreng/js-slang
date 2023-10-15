import { posix as posixPath } from 'path';

import { isSourceModule } from '../../utils/ast/typeGuards';
import { memoizedGetModuleManifestAsync } from "../moduleLoaderAsync"

export type ResolutionOptions = {
  extensions: string[] | null
}

export const defaultResolutionOptions: ResolutionOptions = {
  extensions: ['js']
}

export async function resolveFile(
  source: string,
  filePredicate: (str: string) => Promise<boolean>,
  options: ResolutionOptions
): Promise<[string, boolean]> {
  if (isSourceModule(source)) {
    const manifest = await memoizedGetModuleManifestAsync();
    if (source in manifest) return [source, true]
    return [source, false]
  }

  const absPath = posixPath.resolve(source)

  if (await filePredicate(absPath)) {
    return [absPath, true]
  }

  if (options.extensions) {
    for (const ext of options.extensions) {
      if (await filePredicate(`${absPath}.${ext}`)) {
        return [`${absPath}.${ext}`, true]
      }
    }
  }

  return [source, false]
}