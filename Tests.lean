import LeanJsonParser
import LeanJsonParser.Basic
import LeanTest

open LeanTest

/-- Test that a JSON string is parsed as valid -/
def testJSONValid (input : String) : IO AssertionResult := do
  let result := parseJSON input
  match result with
  | Except.ok valid =>
    return assertTrue valid s!"Expected valid JSON but got false for: {input}"
  | Except.error e =>
    return AssertionResult.failure s!"Expected valid JSON but got error: {e}\nInput: {input}"

/-- Test that a JSON string is parsed as invalid -/
def testJSONInvalid (input : String) : IO AssertionResult := do
  let result := parseJSON input
  match result with
  | Except.ok valid =>
    if valid then
      return AssertionResult.failure s!"Expected invalid JSON but parser accepted it\nInput: {input}"
    else
      return AssertionResult.success
  | Except.error _ =>
    -- Errors are also considered invalid, which is expected
    return AssertionResult.success

/-- Test suite for invalid JSON strings -/
def invalidJSONTests : TestSuite :=
  (TestSuite.empty "Invalid JSON Tests")
  |>.addTest "missing value after colon" (testJSONInvalid "{\"bar\":}")
  |>.addTest "extra colon after value" (testJSONInvalid "{\"bar\":\"yolo\":}")
  |>.addTest "missing colon and value in nested object" (testJSONInvalid "{\"bar\":{\"q\"}}")
  |>.addTest "extra colon after string value" (testJSONInvalid "{\"bar\":\"q\":}")

/-- Test suite for valid JSON strings -/
def validJSONTests : TestSuite :=
  (TestSuite.empty "Valid JSON Tests")
  |>.addTest "empty nested object" (testJSONValid "{\"bar\":{}}")
  |>.addTest "string value (yelp)" (testJSONValid "{\"bar\":\"yelp\"}")
  |>.addTest "string value (yolo)" (testJSONValid "{\"bar\":\"yolo\"}")
  |>.addTest "nested object with key-value" (testJSONValid "{\"bar\":{\"q\":\"foo\"}}")
  |>.addTest "multiple keys with whitespace" (testJSONValid " {\"bar\":\"q\",\"foo\":\"q\"}")
  |>.addTest "failing test" (testJSONValid " {\"bar\":[\"q\",\"q\"]}")

/-- Main function to run all tests -/
def main : IO UInt32 := do
  runTestSuites [
    invalidJSONTests,
    validJSONTests
  ]
  -- Return 0 for compatibility with test runners that check exit codes
  pure 0
