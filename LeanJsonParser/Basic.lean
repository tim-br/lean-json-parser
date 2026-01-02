abbrev Parser := StateT Nat (ExceptT String Id)

partial def skipWhitespace (str : String) : Parser Bool := do
  let pos ← get
  let spos : String.Pos.Raw := ⟨pos⟩
  if spos > str.endPos then
    throw "EOF"
  let char := String.Pos.Raw.get str spos
  match char with
  | ' ' => do
      let nextPos := String.Pos.Raw.next str spos
      set nextPos.byteIdx
      skipWhitespace str
  | '\n' =>
      let nextPos := String.Pos.Raw.next str spos
      set nextPos.byteIdx
      skipWhitespace str
  | _ => pure true

def peekChar (str : String) : Parser Char := do
  _ ← skipWhitespace str
  let pos ← get
  let spos : String.Pos.Raw := ⟨pos⟩
  if spos >= str.endPos then
    throw "EOF"
  else
    pure (String.Pos.Raw.get str spos)

def consumeChar (str : String) : Parser Char := do
  _ ← skipWhitespace str
  let pos ← get
  let spos : String.Pos.Raw := ⟨pos⟩
  if spos >= str.endPos then
    throw "EOF"
  let char := String.Pos.Raw.get str spos
  let nextPos := String.Pos.Raw.next str spos
  set nextPos.byteIdx  -- Extract Nat from String.Pos
  pure char

def consumeCharDontSkipWhitespace (str : String) : Parser Char := do
  let pos ← get
  let spos : String.Pos.Raw := ⟨pos⟩
  if spos >= str.endPos then
    throw "EOF"
  let char := String.Pos.Raw.get str spos
  let nextPos := String.Pos.Raw.next str spos
  set nextPos.byteIdx  -- Extract Nat from String.Pos
  pure char

partial def parseWord (str : String) (res : String) : Parser String := do
  let pos ← get
  let spos : String.Pos.Raw := ⟨pos⟩
  if spos >= str.endPos then
    pure res
  else
    let ch ← consumeCharDontSkipWhitespace str
    if ch == ',' then pure res
    else
    let res' := res ++ ch.toString
    parseWord str res'

def isFloat (s : String) : Bool :=
  if s.isEmpty then false
  else
    let s' := if s.front == '-' then s.drop 1 else s
    let parts := s'.splitToList (· == '.')
    match parts with
    | [intPart] => !intPart.isEmpty && intPart.all Char.isDigit
    | [intPart, fracPart] =>
        !intPart.isEmpty && intPart.all Char.isDigit &&
        !fracPart.isEmpty && fracPart.all Char.isDigit
    | _ => false

mutual

  partial def matchAndReadPair (str : String) (parsingValue? : Bool) : Parser Bool := do
      let c1 ← peekChar str
      match c1 with
      | '{' => readPair str ('{', '}') false
      | '[' => readPair str ('[', ']') parsingValue?
      | '"' => _ <- consumeChar str
               dbg_trace "gets here"
               let res ← readStringOrKey str parsingValue?
               match res with
                | Sum.inl n => Except.error s!"{n}"
                | Sum.inr parsingValue?' => do
                                              dbg_trace s!"matchandreadpair call Alpha {parsingValue?'}"
                                              matchAndReadPair str parsingValue?'
      | _ => if parsingValue? == true
              then
                let w ← parseWord str ""
                dbg_trace s!"w is {w}"
                if w ∈ ["null", "true", "false"] || isFloat w
                then
                  pure true
                  -- let res ← readStringOrKey str false
                  -- dbg_trace s!"res is {res}"
                  -- match res with
                  --   | Sum.inl n => Except.error s!"{n}"
                  --   | Sum.inr parsingValue?' => matchAndReadPair str parsingValue?'
                else Except.error s!"Expected value!"
              else pure true

  partial def readPairAndChar (str : String) (pair : Char × Char) (ch : Char) (parsingValue? : Bool) : Parser Bool := do
    let res1 ← readPair str pair parsingValue?
    dbg_trace "reading char alpha"
    let res2 ← readChar str ch parsingValue?
    if res1 then
      pure res2
    else
      pure false

  partial def readStringOrKey (str : String) (parsingValue? : Bool): Parser (String ⊕ Bool) := do
    let pos ← get

    let spos : String.Pos.Raw := ⟨pos⟩
    if spos >= str.endPos then
      pure (Sum.inl "unexpected EOF")
    else
      let ch ← consumeChar str
      if ch == '"' then
        let pk ← peekChar str
        match pk, parsingValue? with
        | ':', true => do
                        dbg_trace "gets here a"
                        pure (Sum.inl "unexpected :")
        | ':', false => do
                          _ ← consumeChar str
                          pure (Sum.inr true)
        | ',', true => do
                         _ ← consumeChar str
                         pure (Sum.inr false)
        | ',', false => pure (Sum.inl "unexpected ,")
        | _, false => pure (Sum.inl "expected : ")
        | _, _ => pure (Sum.inr false)
      else
        readStringOrKey str parsingValue?

  partial def readChar (str : String) (ch : Char) (parsingValue? : Bool) : Parser Bool := do
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
          readPairAndChar str ('{', '}') ch false
        | '[' =>
          readPairAndChar str ('[', ']') ch parsingValue?
        | '"' =>
          _ ← consumeChar str
          dbg_trace "here"
          dbg_trace s!" parsing value z : {parsingValue?}"
          let res ← readStringOrKey str parsingValue?
          match res with
          | Sum.inl n => Except.error s!"{n}"
          | Sum.inr parsingValue?' =>
            if parsingValue?' then

              let res ← matchAndReadPair str parsingValue?'
              if res then
                dbg_trace s!"read char v {parsingValue?'}"
                let res2 ← (readChar str ch false)
                pure res2
              else
                pure res
            else
              readChar str ch parsingValue?'
        | ',' =>
          dbg_trace " got ,"
          _ ← consumeChar str
          let quote ← peekChar str
          if quote != '"' then
            Except.error s!"Expected \" actual {c1}!"
          else
            dbg_trace "matchandReadPair call"
            let res ← matchAndReadPair str false
            if res then
              dbg_trace "if statement"
              readChar str ch false
            else
              pure false
        | _ =>
            if parsingValue? then
              Except.error s!"unexpected key {c1}"
            else
            dbg_trace s!"parsing remainder {ch}"
            _ ← consumeChar str
            let res ← readChar str ch parsingValue?
            pure res

  partial def readPair (str : String) (pair : Char × Char) (parsingValue? : Bool) : Parser Bool := do
      let c1 ← consumeChar str
      if c1 != pair.fst then
        Except.error s!"Expected: {pair.fst}, actual {c1}!"
      else
        dbg_trace "reading char zeta"
        readChar str pair.snd parsingValue?
end

def parseJSON (str : String) : Except String Bool := do
  let result ← (do
    let res ← matchAndReadPair str false
    _ ← skipWhitespace str
    let pos ← get
    let spos: String.Pos.Raw := ⟨pos⟩
    if  spos < str.endPos then
      pure false
    else
    pure res
  ).run 0
  pure result.1
