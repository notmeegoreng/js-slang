import { Chapter } from '../types'
import { expectParsedError, expectResultsToEqual } from '../utils/testing/testers'

expectResultsToEqual([
  [
    'standalone block statements',
    `
      function test(){
        const x = true;
        {
            const x = false;
        }
        return x;
      }
      test();
    `,
    true
  ],
  [
    'const uses block scoping instead of function scoping',
    `
      function test(){
        const x = true;
        if(true) {
            const x = false;
        } else {
            const x = false;
        }
        return x;
      }
      test();
    `,
    true
  ],
  [
    'let uses block scoping instead of function scoping',
    `
      function test(){
        let x = true;
        if(true) {
            const x = false;
        } else {
            const x = false;
        }
        return x;
      }
      test();
    `,
    true
  ],
  [
    'for loops use block scoping instead of function scoping',
    `
      function test(){
        let x = true;
        for (let x = 1; x > 0; x = x - 1) {
        }
        return x;
      }
      test();
    `,
    true
  ],
  [
    'while loops use block scoping instead of function scoping',
    `
      function test(){
        let x = true;
        while (true) {
          let x = false;
          break;
        }
        return x;
      }
      test();
    `,
    true
  ],
  // see https://www.ecma-international.org/ecma-262/6.0/#sec-for-statement-runtime-semantics-labelledevaluation
  // and https://hacks.mozilla.org/2015/07/es6-in-depth-let-and-const/
  [
    'for loop `let` variables are copied into the block scope',
    `
      function test(){
        let z = [];
        for (let x = 0; x < 10; x = x + 1) {
          z[x] = () => x;
        }
        return z[1]();
      }
      test();
    `,
    1
  ]
], Chapter.SOURCE_4)

test('Cannot overwrite loop variables within a block', () => {
  return expectParsedError(
    `
      function test(){
          let z = [];
          for (let x = 0; x < 2; x = x + 1) {
            x = 1;
          }
          return false;
      }
      test();
    `,
    Chapter.SOURCE_3
  ).toEqual("Line 5: Assignment to a for loop variable in the for loop is not allowed.")
})

// expectParsedErrorsToEqual([
//   [
//     'Cannot overwrite loop variables within a block',
//     `
//       function test(){
//           let z = [];
//           for (let x = 0; x < 2; x = x + 1) {
//             x = 1;
//           }
//           return false;
//       }
//       test();
//     `,
//     "Line 4: Assignment to the loop variable in the for loop is not allowed."
//   ],
//   [
//     'No hoisting of functions. Only the name is hoisted like let and const',
//     `
//       const v = f();
//       function f() {
//         return 1;
//       }
//       v;
//     `,
//     "Line 1: Name f declared later in current scope but not yet assigned"
//   ]
// ], Chapter.SOURCE_4)

// test('Error when accessing temporal dead zone', () => {
//   return expectParsedError(stripIndent`
//     const a = 1;
//     function f() {
//       display(a);
//       const a = 5;
//     }
//     f();
//     `).toMatchInlineSnapshot(
//     `"Line 3: Name a declared later in current scope but not yet assigned"`
//   )
// }, 30000)

// // tslint:disable-next-line:max-line-length
// test('In a block, every going-to-be-defined variable in the block cannot be accessed until it has been defined in the block.', () => {
//   return expectParsedError(stripIndent`
//       const a = 1;
//       {
//         a + a;
//         const a = 10;
//       }
//     `).toMatchInlineSnapshot(
//     `"Line 3: Name a declared later in current scope but not yet assigned"`
//   )
// }, 30000)

test('Shadowed variables may not be assigned to until declared in the current scope', () => {
  return expectParsedError(
    `
      let variable = 1;
      function test(){
        variable = 100;
        let variable = true;
        return variable;
      }
      test();
    `,
    Chapter.SOURCE_3
  ).toEqual("Line 4: Name variable not declared.")
})
