import LeanJsonParser
import LeanJsonParser.Basic

mutual

  partial def matchAndReadPair (str : String) : Parser Bool := do
      let c1 ← peekChar str
      match c1 with
      | '{' => readPair str ('{', '}')
      | '[' => readPair str ('[', ']')
      | _ => Except.error s!"Expected, '\{' or '[', actual {c1}!"

  partial def readPairAndChar (str : String) (pair : Char × Char) (ch : Char) : Parser Bool := do
    let res1 ← readPair str pair
    let res2 ← readChar str ch
    if res1 then
      pure res2
    else
      pure false

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
          readPairAndChar str ('{', '}') ch
        | '[' =>
          readPairAndChar str ('[', ']') ch
        | '"' =>
          _ ← consumeChar str
          let res1 ← readStringOrKey str
          let c1 ← peekChar str
          dbg_trace s!"c1  = {c1}"
          if c1 == ':' then
            _ ← consumeChar str
            let res1 ← matchAndReadPair str
            let res2 ← readChar str ch
            if res1 then
              pure res2
            else
              pure false
          else
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
    let res ← matchAndReadPair str
    let pos ← get
    let spos: String.Pos.Raw := ⟨pos⟩
    if  spos < str.endPos then
    dbg_trace "returning false"
    pure false
    else
    pure res
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
