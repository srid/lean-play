import examples.MessageSelf.Storage
import examples.MessageSelf.Terminal

namespace MessageSelf

private def defaultPath : System.FilePath := ".messages.jsonl"

private def usage : String :=
  "Usage: MessageSelf [FILE]\n\n" ++
  "FILE defaults to .messages.jsonl.\n"

private structure State where
  entries : Array Entry
  input : String := ""
  scroll : Nat := 0
  size : Terminal.Dimensions
  today : String
  expandedDays : Array String := #[]

private def toggleDay (days : Array String) (day : String) : Array String :=
  if days.contains day then days.filter (· != day) else days.push day

private def anchoredScroll
    (state : State) (expandedDays : Array String) : Nat :=
  let before := Terminal.maxScroll state.size state.entries state.today state.expandedDays
  let after := Terminal.maxScroll state.size state.entries state.today expandedDays
  if before ≤ after then state.scroll + (after - before)
  else state.scroll - (before - after)

private partial def eventLoop
    (tty : IO.FS.Handle) (path : System.FilePath) (state : State)
    (redraw := true) : IO Unit := do
  if redraw then
    Terminal.render state.size path state.entries state.today state.expandedDays
      state.input state.scroll

  match ← Terminal.readKey tty with
  | .idle =>
      let size ← Terminal.dimensions
      eventLoop tty path { state with size } (size != state.size)
  | .quit => pure ()
  | .text text =>
      eventLoop tty path { state with input := state.input ++ text }
  | .backspace =>
      eventLoop tty path { state with input := state.input.dropEnd 1 |>.toString }
  | .clearInput =>
      eventLoop tty path { state with input := "" }
  | .scrollUp =>
      let limit := Terminal.maxScroll state.size state.entries state.today state.expandedDays
      eventLoop tty path { state with scroll := min limit (state.scroll + 1) }
  | .scrollDown =>
      eventLoop tty path { state with scroll := state.scroll - 1 }
  | .wheelUp =>
      let limit := Terminal.maxScroll state.size state.entries state.today state.expandedDays
      eventLoop tty path { state with scroll := min limit (state.scroll + 3) }
  | .wheelDown =>
      eventLoop tty path { state with scroll := state.scroll - 3 }
  | .pageUp =>
      let limit := Terminal.maxScroll state.size state.entries state.today state.expandedDays
      let page := Terminal.viewportHeight state.size
      eventLoop tty path { state with scroll := min limit (state.scroll + page) }
  | .pageDown =>
      let page := Terminal.viewportHeight state.size
      eventLoop tty path { state with scroll := state.scroll - page }
  | .click row _ =>
      match Terminal.dayAtRow state.size state.entries state.today state.expandedDays
          state.scroll row with
      | some day =>
          let expandedDays := toggleDay state.expandedDays day
          let scroll := anchoredScroll state expandedDays
          eventLoop tty path { state with expandedDays, scroll }
      | none => eventLoop tty path state false
  | .enter =>
      let message := state.input.trimAscii.toString
      match message with
      | "" =>
          eventLoop tty path { state with input := "", scroll := 0 }
      | _ =>
          let entry ← Entry.create message
          Storage.append path entry
          let today ← currentDay
          let expandedDays :=
            if today == state.today then state.expandedDays else #[today]
          eventLoop tty path {
            state with
            entries := state.entries.push entry
            input := ""
            scroll := 0
            today
            expandedDays
          }

def run (path : System.FilePath) : IO Unit := do
  let entries ← Storage.load path
  Terminal.withTerminal fun tty => do
    let size ← Terminal.dimensions
    let today ← currentDay
    eventLoop tty path { entries, size, today, expandedDays := #[today] }

end MessageSelf

def main (args : List String) : IO UInt32 := do
  try
    match args with
    | [] => MessageSelf.run MessageSelf.defaultPath
    | ["--help"] | ["-h"] => IO.print MessageSelf.usage
    | [path] => MessageSelf.run path
    | _ => throw (IO.userError "expected at most one JSONL file")
    pure 0
  catch error =>
    IO.eprintln s!"MessageSelf: {error}\n\n{MessageSelf.usage}"
    pure 1
