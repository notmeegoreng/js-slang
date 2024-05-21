import { Chapter } from '../types'
import { expectParsedError, expectResult } from '../utils/testing/testers'

test('draw_data returns first argument if more than one argument', () => {
  return expectResult(`draw_data(1, 2);`, Chapter.SOURCE_3).toEqual(1)
})

test('draw_data returns first argument if exactly one argument', () => {
  return expectResult(`draw_data(1);`, Chapter.SOURCE_3).toEqual(1)
})

test('draw_data with no arguments throws error', () => {
  return expectParsedError(`draw_data();`, Chapter.SOURCE_3).toEqual(
    "Line 1: Expected 1 or more arguments, but got 0."
  )
})
