abbrev Parser := StateT Nat (ExceptT String Id)

def peekChar (str : String) : Parser Char := do
  let pos ← get
  let spos : String.Pos.Raw := ⟨pos⟩
  if spos >= str.endPos then
    throw "EOF"
  else
    pure (String.Pos.Raw.get str spos)

def consumeChar (str : String) : Parser Char := do
  let pos ← get
  let spos : String.Pos.Raw := ⟨pos⟩
  if spos >= str.endPos then
    throw "EOF"
  let char := String.Pos.Raw.get str spos
  let nextPos := String.Pos.Raw.next str spos
  set nextPos.byteIdx  -- Extract Nat from String.Pos
  pure char

mutual

  partial def matchAndReadPair (str : String) (parsingValue? : Bool) : Parser Bool := do
      let c1 ← peekChar str
      dbg_trace s!"matching and reading {c1} parsingValue? {parsingValue?}"
      match c1 with
      | '{' => readPair str ('{', '}') false
      | '[' => readPair str ('[', ']') parsingValue?
      | '"' => let y ← peekChar str
               dbg_trace s!"y {y}"
               let pos <- get
               dbg_trace s!"pos y {pos}"
               _ <- consumeChar str
               let res ← readStringOrKey str parsingValue?
               let next ← peekChar str
               dbg_trace s!"next {next}"
               match res with
                | Sum.inl n => Except.error s!"{n}"
                | Sum.inr parsingValue?' => pure true--matchAndReadPair str parsingValue?'
      | _ => Except.error s!"Expected '\{' or '[' or \", actual {c1}!"

  partial def readPairAndChar (str : String) (pair : Char × Char) (ch : Char) (parsingValue? : Bool) : Parser Bool := do
    let res1 ← readPair str pair parsingValue?
    let res2 ← readChar str ch parsingValue?
    if res1 then
      pure res2
    else
      pure false

  partial def readStringOrKey (str : String) (parsingValue? : Bool): Parser (String ⊕ Bool) := do
    dbg_trace "read string or key"
    let pos ← get
    dbg_trace s!"pos = {pos}"

    let spos : String.Pos.Raw := ⟨pos⟩
    if spos >= str.endPos then
      pure (Sum.inl "unexpected EOF")
    else
      let ch ← consumeChar str
      dbg_trace s!"read string or key {ch}"
      if ch == '"' then
        dbg_trace "end of string"
        let pk ← peekChar str
        dbg_trace s!"parsing value {parsingValue?}"
        match pk, parsingValue? with
        | ':', true => pure (Sum.inl "unexpected :")
        | ':', false => do
                          _ ← consumeChar str
                          dbg_trace "switching to true"
                          let pk ← peekChar str
                          dbg_trace s! "pk {pk}"
                          pure (Sum.inr true)
        | _, false => pure (Sum.inl "expected :")
        | _, _ => pure (Sum.inr false)
      else
        readStringOrKey str parsingValue?

  partial def readChar (str : String) (ch : Char) (parsingValue? : Bool) : Parser Bool := do
    dbg_trace s!"parsing value k {parsingValue?}"
    let c1 ← peekChar str
    dbg_trace s!"c1 k {c1}"
    dbg_trace s!"ch k {ch}"
    let pos ← get
    let spos : String.Pos.Raw := ⟨pos⟩
    if spos >= str.endPos then
      pure false
    else
      if c1 == ch then
        dbg_trace "test"
        dbg_trace "end"
        _ ← consumeChar str
        pure true
      else
        match c1 with
        | '{' =>
          readPairAndChar str ('{', '}') ch false
        | '[' =>
          readPairAndChar str ('[', ']') ch parsingValue?
        | '"' =>
          dbg_trace "quote"
          let ch2 ← consumeChar str
          dbg_trace s!"ch2 {ch2}"
          let res ← readStringOrKey str parsingValue?
          match res with
          | Sum.inl n => Except.error s!"{n}"
          | Sum.inr parsingValue?' =>
            if parsingValue?' then
              dbg_trace "match and read a"
              let res ← matchAndReadPair str parsingValue?'
              if res then
                let res2 ← (readChar str ch parsingValue?')
                pure res2
              else
                pure res
            else
              readChar str ch parsingValue?'

        | _ =>
            if parsingValue? then
              Except.error s!"unexpected key {c1}"
            else
            dbg_trace s!"parsing remainder {ch}"
            _ ← consumeChar str
            let c1 ← peekChar str
            dbg_trace s!"peek {c1}"
            let res ← readChar str ch parsingValue?
            pure res

  partial def readPair (str : String) (pair : Char × Char) (parsingValue? : Bool) : Parser Bool := do
      let c1 ← consumeChar str
      dbg_trace s!" c1 {c1}"
      if c1 != pair.fst then
        --pure false
        Except.error s!"Expected, {pair.fst}, actual {c1}!"
      else
        dbg_trace "right before"
        readChar str pair.snd parsingValue?
end

def parseJSON (str : String) : Except String Bool := do
  let result ← (do
    let res ← matchAndReadPair str false
    let pos ← get
    let spos: String.Pos.Raw := ⟨pos⟩
    if  spos < str.endPos then
      dbg_trace "returning false"
      pure false
    else
    pure res
  ).run 0
  pure result.1
