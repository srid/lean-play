import examples.MessageSelf.Model

namespace MessageSelf.Storage

private def decode (line : String) : Except String Entry := do
  let json ← Lean.Json.parse line
  Lean.fromJson? json

def load (path : System.FilePath) : IO (Array Entry) := do
  unless ← path.pathExists do
    return #[]

  let lines ← IO.FS.lines path
  let mut entries := #[]
  for line in lines do
    if !line.trimAscii.isEmpty then
      match decode line with
      | .ok entry => entries := entries.push entry
      | .error error =>
          throw (IO.userError s!"invalid JSONL in {path}: {error}")
  pure entries

def append (path : System.FilePath) (entry : Entry) : IO Unit :=
  IO.FS.withFile path .append fun handle =>
    handle.putStrLn (Lean.Json.compress (Lean.toJson entry))

end MessageSelf.Storage
