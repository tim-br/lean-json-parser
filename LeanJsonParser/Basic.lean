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
