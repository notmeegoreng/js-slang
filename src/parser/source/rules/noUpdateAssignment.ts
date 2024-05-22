import { generate } from 'astring'
import type es from 'estree'

import { UNKNOWN_LOCATION } from '../../../constants'
import { ErrorSeverity, ErrorType, type SourceError } from '../../../types'
import type { Rule } from '../../types'

export class NoUpdateAssignment implements SourceError {
  public type = ErrorType.SYNTAX
  public severity = ErrorSeverity.ERROR

  constructor(public node: es.AssignmentExpression) {}

  get location() {
    return this.node.loc ?? UNKNOWN_LOCATION
  }

  public explain() {
    return 'The assignment operator ' + this.node.operator + ' is not allowed. Use = instead.'
  }

  public elaborate() {
    const leftStr = generate(this.node.left)
    const rightStr = generate(this.node.right)
    const newOpStr = this.node.operator.slice(0, -1)

    if (newOpStr === '+' || newOpStr === '-' || newOpStr === '/' || newOpStr === '*') {
      const elabStr = `\n\t${leftStr} = ${leftStr} ${newOpStr} ${rightStr};`

      return elabStr
    } else {
      return ''
    }
  }
}

const disallowedAssignmentOperators: es.AssignmentOperator[] = [
  // Some operators aren't recognized as valid operators
  '+=',
  '-=',
  '*=',
  '/=',
  '%=',
  // "**=",
  '<<=',
  '>>=',
  '>>>=',
  '|=',
  '^=',
  '&='
  // "||=",
  // "&&=",
  // "??="
]

const testSnippets = disallowedAssignmentOperators.map(
  op =>
    [`a ${op} b;`, `Line 1: The assignment operator ${op} is not allowed. Use = instead.`] as [
      string,
      string
    ]
)

const noUpdateAssignment: Rule<es.AssignmentExpression> = {
  name: 'no-update-assignment',
  testSnippets,

  checkers: {
    AssignmentExpression(node: es.AssignmentExpression) {
      if (node.operator !== '=') {
        return [new NoUpdateAssignment(node)]
      } else {
        return []
      }
    }
  }
}

export default noUpdateAssignment
