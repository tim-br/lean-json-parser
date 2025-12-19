import LeanJsonParser
import LeanJsonParser.Basic

def main : IO UInt32 := do
  let stdin ← IO.getStdin
  let contents ← stdin.readToEnd
  let res := (parseJSON contents)
  match res with
  | Except.ok true =>
    IO.println "valid JSON!"
    pure 0
  | Except.ok false =>
    IO.println "invalid JSON :( but no errors thrown"
    pure 0
  | Except.error e =>
    IO.println s!"invalid JSON :( {e}"
    pure 1
