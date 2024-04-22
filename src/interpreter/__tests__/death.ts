import { Chapter } from "../../types"
import { stripIndent } from "../../utils/formatters"
import { expectParsedError } from "../../utils/testing"

test('Error when redeclaring function after let', () => {
  return expectParsedError(
    stripIndent`
    let f = x => x;
    function f() {}
  `,
    { chapter: Chapter.SOURCE_3, native: true }
  ).toMatchInlineSnapshot(`"Line 2: SyntaxError: Identifier 'f' has already been declared (2:9)"`)
})