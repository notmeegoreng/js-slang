import type * as es from 'estree'

import { RuntimeSourceError } from '../errors/runtimeSourceError'

export class UndefinedNamespaceImportError extends RuntimeSourceError {
  constructor(
    public readonly moduleName: string,
    node?: es.Node
  ) {
    super(node)
  }

  public explain(): string {
    return `'${this.moduleName}' does not have any exports!`
  }

  public elaborate(): string {
    return "Check your imports and make sure what you're trying to import exists!"
  }
}

export class UndefinedImportError extends UndefinedNamespaceImportError {
  constructor(
    public readonly symbol: string,
    moduleName: string,
    node?: es.ImportDefaultSpecifier | es.ImportSpecifier | es.ExportSpecifier
  ) {
    super(moduleName, node)
  }

  public explain(): string {
    return `'${this.moduleName}' does not export the symbol '${this.symbol}'`
  }
}

export class UndefinedDefaultImportError extends UndefinedImportError {
  constructor(
    moduleName: string,
    node?: es.ImportDefaultSpecifier | es.ImportSpecifier | es.ExportSpecifier
  ) {
    super(
      'default',
      moduleName,
      node
    )
  }

  public explain(): string {
    return `'${this.moduleName}' does not have a default export!`
  }
}

export class ModuleConnectionError extends RuntimeSourceError {
  private static message: string = `Unable to get modules.`
  private static elaboration: string = `You should check your Internet connection, and ensure you have used the correct module path.`
  constructor(node?: es.Node) {
    super(node)
  }

  public explain() {
    return ModuleConnectionError.message
  }

  public elaborate() {
    return ModuleConnectionError.elaboration
  }
}

export class ModuleNotFoundError extends RuntimeSourceError {
  constructor(public moduleName: string, node?: es.Node) {
    super(node)
  }

  public explain() {
    return `Module "${this.moduleName}" not found.`
  }

  public elaborate() {
    return `
      You should check your import declarations, and ensure that all are valid modules.
    `
  }
}

export class ModuleInternalError extends RuntimeSourceError {
  constructor(public moduleName: string, public error?: any, node?: es.Node) {
    super(node)
  }

  public explain() {
    return `Error(s) occured when executing the module "${this.moduleName}".`
  }

  public elaborate() {
    return `
      You may need to contact with the author for this module to fix this error.
    `
  }
}
