import LeanJsonParser
import LeanJsonParser.Basic

def testJSON (input : String) (expected : Bool) (name : String) : IO Bool := do
  let result := parseJSON input
  match result with
  | Except.ok valid =>
    if valid == expected then
      IO.println s!"✓ {name}: PASS (got {valid}, expected {expected})"
      pure true
    else
      IO.println s!"✗ {name}: FAIL (got {valid}, expected {expected})"
      pure false
  | Except.error e =>
    if expected == false then
      IO.println s!"✓ {name}: PASS (errored as expected: {e})"
      pure true
    else
      IO.println s!"✗ {name}: FAIL (errored when should succeed: {e})"
      pure false

def runTests : IO UInt32 := do
  IO.println "Running JSON Parser Tests..."
  IO.println "============================\n"

  let mut allPassed := true

  -- Test 2: Invalid JSON - missing value after "bar":
  let test2 := "{\"bar\":}"
  let res2 ← testJSON test2 false "test2 (invalid - missing value)"
  allPassed := allPassed && res2

  -- Test 3: Currently fails - parser bug with colon handling
  let test3 := "{\"bar\":{}}"
  let res3 ← testJSON test3 true "test3 (should be valid - empty object, but parser fails)"
  allPassed := allPassed && res3

  -- Test 4: Currently fails - parser bug with colon handling
  let test4 := "{\"bar\":\"yelp\"}"
  let res4 ← testJSON test4 true "test4 (should be valid - string value, but parser fails)"
  allPassed := allPassed && res4

  -- Test 5: Currently fails - parser bug with colon handling
  let test5 := "{\"bar\":\"yolo\"}"
  let res5 ← testJSON test5 true "test5 (should be valid - string value, but parser fails)"
  allPassed := allPassed && res5

  -- Test 6: Invalid JSON - extra colon
  let test6 := "{\"bar\":\"yolo\":}"
  let res6 ← testJSON test6 false "test6 (invalid - extra colon)"
  allPassed := allPassed && res6

  -- Test 7: Currently fails - parser bug with colon handling
  let test7 := "{\"bar\":{\"q\":\"foo\"}}"
  let res7 ← testJSON test7 true "test7 (should be valid - nested object, but parser fails)"
  allPassed := allPassed && res7

  -- Test 8: Invalid JSON - missing colon and value in nested object
  let test8 := "{\"bar\":{\"q\"}}"
  let res8 ← testJSON test8 false "test8 (invalid - missing colon and value)"
  allPassed := allPassed && res8

  -- Test 9: Invalid JSON - extra colon
  let test9 := "{\"bar\":\"q\":}"
  let res9 ← testJSON test9 false "test9 (invalid - extra colon)"
  allPassed := allPassed && res9

  IO.println "\n============================\n"
  if allPassed then
    IO.println "All tests passed!"
    pure 0
  else
    IO.println "Some tests failed!"
    pure 1

def main : IO UInt32 := runTests
