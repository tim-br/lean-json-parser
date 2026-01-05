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

  partial def readList (str : String) : Parser Bool := do
      let c1 ← consumeChar str

      if c1 ≠ '[' then
        Except.error s!"Expected: [, actual {c1}!"
      else
        let c2 ← peekChar str
        if c2 == ']' then
          pure true
        else
          readListHelper str

      where readListHelper (str : String) := do

        let res ← matchAndReadValueInList str
        if res then
          let c2 ← peekChar str
          if c2 == ']' then
            _ ← consumeChar str
            pure true
          else
            if c2 ≠ ',' then
              readListHelper str
            else
              _ ← consumeChar str
              readListHelper str
        else
          pure false

  partial def matchAndReadValueInList (str : String) : Parser Bool := do
      let c1 ← peekChar str
      match c1 with
      | '{' => readPair str ('{', '}') false
      | '[' => readList str
      | '"' => _ <- consumeChar str
               let res ← readStringOrKey str true true
               match res with
                | Sum.inl n => Except.error s!"{n}"
                | Sum.inr _parsingValue?' => pure true
      -- | ',' =>
      --         pure true
      --| ']' => pure false
      | _ => if true
              then
                let w ← parseWord str ""
                if w ∈ ["null", "true", "false"] || isFloat w
                then
                  pure true
                  -- let res ← readStringOrKey str false
                  --  s!"res is {res}"
                  -- match res with
                  --   | Sum.inl n => Except.error s!"{n}"
                  --   | Sum.inr parsingValue?' => matchAndReadPair str parsingValue?'
                else
                  Except.error s!"Expected value! got {w}"
              else pure true

  partial def matchAndReadPair (str : String) (parsingValue? : Bool) : Parser Bool := do
      let c1 ← peekChar str
      match c1 with
      | '{' => readPair str ('{', '}') false
      | '[' => readList str
      | '"' => _ <- consumeChar str
               let res ← readStringOrKey str parsingValue? false
               match res with
                | Sum.inl n => Except.error s!"{n}"
                | Sum.inr parsingValue?' => do
                                              matchAndReadPair str parsingValue?'
      | _ => if parsingValue? == true
              then
                let w ← parseWord str ""
                if w ∈ ["null", "true", "false"] || isFloat w
                then
                  pure true
                  -- let res ← readStringOrKey str false
                  --  s!"res is {res}"
                  -- match res with
                  --   | Sum.inl n => Except.error s!"{n}"
                  --   | Sum.inr parsingValue?' => matchAndReadPair str parsingValue?'

                else
                  Except.error s!"Expected value!"
              else pure true

  partial def readPairAndChar (str : String) (pair : Char × Char) (ch : Char) (parsingValue? : Bool) : Parser Bool := do
    let res1 ← readPair str pair parsingValue?
    let res2 ← readChar str ch parsingValue?
    if res1 then
      pure res2
    else
      pure false

  partial def readStringOrKey (str : String) (parsingValue? : Bool) (parsingList? : Bool): Parser (String ⊕ Bool) := do
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
                        pure (Sum.inl "unexpected :")
        | ':', false => do
                          _ ← consumeChar str
                          pure (Sum.inr true)
        | ',', true => do
                         if parsingList? then

                          pure (Sum.inr true)
                         else
                          _ ← consumeChar str
                          pure (Sum.inr false)
        | ',', false => pure (Sum.inl "unexpected ,")
        | ']', _ => do
                        --_ ← consumeChar str
                        pure (Sum.inr true)
        | _, false => pure (Sum.inl s!"expected : or , or ] actual {pk}")
        | _, _ => pure (Sum.inr false)
      else
        readStringOrKey str parsingValue? parsingList?

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
        | '[' => do
            let res ← readList str
            if res == true then
              let ch1 ← consumeChar str
              if ch1 = ch then
                pure true
              else
                pure false
            else
              pure false
        | '"' =>
          _ ← consumeChar str
          let res ← readStringOrKey str parsingValue? false
          match res with
          | Sum.inl n => Except.error s!"{n}"
          | Sum.inr parsingValue?' =>
            if parsingValue?' then
              let res ← matchAndReadPair str parsingValue?'
              if res then
                let res2 ← (readChar str ch false)
                pure res2
              else
                pure res
            else
              readChar str ch parsingValue?'
        | ',' =>
          _ ← consumeChar str
          let res ← matchAndReadPair str false
          if res then
            readChar str ch false
          else
            pure false

          -- _ ← consumeChar str
          -- let quote ← peekChar str
          -- if quote != '"' then
          --   Except.error s!"Expected \" actual {quote}!"
          -- else
          --   let res ← matchAndReadPair str false
          --   if res then
          --     readChar str ch false
          --   else
          --     pure false
        | _ =>
            if parsingValue? then
              Except.error s!"unexpected key {c1}"
            else
              _ ← consumeChar str
              let res ← readChar str ch parsingValue?
              pure res

  partial def readPair (str : String) (pair : Char × Char) (parsingValue? : Bool) : Parser Bool := do
      let c1 ← consumeChar str
      if c1 != pair.fst then
        Except.error s!"Expected: {pair.fst}, actual {c1}!"
      else

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
