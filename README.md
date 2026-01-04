# lean-json-parser

A minimal JSON parser implementation in Lean 4 as a response to the Build Your Own JSON Parser challenge (https://codingchallenges.fyi/challenges/challenge-json-parser).

## Usage

```bash
cat tests/mytests/test3.json | lake exe lean-json-parser
# valid JSON!
```

## Running Tests

```bash
lake exe tests
```

This runs the test suite defined in `Tests.lean`, which includes tests for both valid and invalid JSON parsing.
