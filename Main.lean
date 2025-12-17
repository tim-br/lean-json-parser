import LeanJsonParser

partial def readBytes (stream : IO.FS.Stream) (byteCode : Nat) : IO (Bool) := do
  let byte ← stream.read 1
  if byte.isEmpty then
    pure False
  else
    if byte[0]!.toNat == byteCode then
      pure True
    else
      readBytes stream byteCode

def main : IO UInt32 := do
  let stdin ← IO.getStdin
  let byte ← stdin.read 1
  if !byte.isEmpty then
    if byte[0]!.toNat == 123 then
      let ret ← readBytes stdin 125
      if ret then
        return 0
      else
        return 1
    else
      return 1
  else
    return 1
