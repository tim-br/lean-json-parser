# lean-json-parser

A minimal JSON parser implementation in Lean 4. Currently supports parsing empty objects (`{}`) and strings.

## Usage

```bash
cat test.json | tr -d '\n' | lake exe lean-json-parser
```

**Note:** Newlines must be removed from the input using `tr -d '\n'` before parsing.
