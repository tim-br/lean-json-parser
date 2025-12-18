import LeanJsonParser
import LeanJsonParser.Basic

mutual
  partial def readStringOrKey (str : String): Parser Bool := do
    let pos ← get
    let spos : String.Pos.Raw := ⟨pos⟩
    if spos >= str.endPos then
      pure false
    else
      let ch ← consumeChar str
      if ch == '"' then
        pure true
      else
        readStringOrKey str

  partial def readChar (str : String) (ch : Char) : Parser Bool := do
    let c1 ← peekChar str
    let pos ← get
    let spos : String.Pos.Raw := ⟨pos⟩
    if spos >= str.endPos then
      pure false
    else
      if c1 == ch then
        _ ← consumeChar str
        pure true
      else
        match c1 with
        | '{' =>
          let res1 ← readPair str ('{', '}')
          let res2 ← readChar str ch
          if res1 then
            pure res2
          else
            pure false
        | '[' =>
          let res1 ← readPair str ('[', ']')
          let res2 ← readChar str ch
          if res1 then
            pure res2
          else
            pure false
        | '"' =>
          _ ← consumeChar str
          let res1 ← readStringOrKey str
          let res2 ← readChar str ch
          if res1 then
            pure res2
          else
            pure false
        | _ =>
          _ ← consumeChar str
          let res ← readChar str ch
          pure res

  partial def readPair (str : String) (pair : Char × Char) : Parser Bool := do
      let c1 ← consumeChar str
      if c1 != pair.fst then
        --pure false
        Except.error s!"Expected, {pair.fst}, actual {c1}!"
      else
        readChar str pair.snd
end

def parseJSON (str : String) : Except String Bool := do
  let result ← (do
    let c1 ← peekChar str
    match c1 with
    | '{' => let res ← readPair str ('{', '}')
             let pos ← get
             let spos: String.Pos.Raw := ⟨pos⟩
             if  spos < str.endPos then
              pure false
             else
             pure res
    | _ => pure false
  ).run 0
  pure result.1

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
