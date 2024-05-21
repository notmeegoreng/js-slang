import { Chapter } from '../../types'
import { stripIndent } from '../../utils/formatters'
import { expectParsedError } from '../../utils/testing/testers'

test('Blatant syntax error', () => {
  return expectParsedError(
    stripIndent`
    stringify(parse("'"), undefined, 2);
    `,
    Chapter.SOURCE_4
  ).toEqual("Line 1: ParseError: SyntaxError: Unterminated string constant (1:0)")
})

test('Blacklisted syntax', () => {
  return expectParsedError(
    stripIndent`
    stringify(parse("function* f() { yield 1; } f();"), undefined, 2);
    `,
    Chapter.SOURCE_4
  ).toEqual("Line 1: ParseError: Yield expressions are not allowed.")
})
