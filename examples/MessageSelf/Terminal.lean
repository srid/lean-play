import examples.MessageSelf.Model

namespace MessageSelf.Terminal

namespace Ansi

def reset : String := "\x1b[0m"
def home : String := "\x1b[H"
def enterScreen : String := "\x1b[?1049h\x1b[2J\x1b[H"
def leaveScreen : String := "\x1b[?1049l"
def hideCursor : String := "\x1b[?25l"
def showCursor : String := "\x1b[?25h"
def enableMouse : String := "\x1b[?1000h\x1b[?1002h\x1b[?1006h"
def disableMouse : String := "\x1b[?1000l\x1b[?1002l\x1b[?1006l"

def styled (code text : String) : String := s!"\x1b[{code}m{text}{reset}"
def dim (text : String) : String := styled "2" text
def cyan (text : String) : String := styled "38;5;81" text
def green (text : String) : String := styled "38;5;114" text
def amber (text : String) : String := styled "38;5;215" text
def violet (text : String) : String := styled "38;5;141" text

end Ansi

structure Dimensions where
  rows : Nat
  columns : Nat
deriving BEq, Repr

inductive Key where
  | text (value : String)
  | enter
  | backspace
  | clearInput
  | scrollUp
  | scrollDown
  | wheelUp
  | wheelDown
  | pageUp
  | pageDown
  | mouseDown (row column : Nat)
  | mouseDrag (row column : Nat)
  | mouseUp
  | quit
  | idle
deriving Repr

private def repeatText (count : Nat) (text : String) : String :=
  String.join (List.replicate count text)

private def clip (width : Nat) (text : String) : String :=
  (text.take width).toString

private def takeRight (width : Nat) (text : String) : String :=
  if text.length ≤ width then text
  else (text.drop (text.length - width)).toString

private def padRight (width : Nat) (text : String) : String :=
  let clipped := clip width text
  clipped ++ repeatText (width - clipped.length) " "

private def border (width : Nat) (left fill right : String) : String :=
  left ++ repeatText (width - 2) fill ++ right

private def panelLine (width : Nat) (content : String) : String :=
  "│" ++ padRight (width - 2) content ++ "│"

private structure WrapState where
  linesRev : List String := []
  current : String := ""

private def WrapState.push (width : Nat) (state : WrapState) (word : String) : WrapState :=
  if state.current.isEmpty then
    { state with current := word }
  else if state.current.length + word.length + 1 ≤ width then
    { state with current := state.current ++ " " ++ word }
  else
    { linesRev := state.current :: state.linesRev, current := word }

private def wrapText (width : Nat) (text : String) : List String :=
  let state := (text.splitOn " ").foldl (WrapState.push width) {}
  (if state.current.isEmpty then state.linesRev else state.current :: state.linesRev).reverse

private def write (text : String) : IO Unit := do
  let stdout ← IO.getStdout
  stdout.putStr text
  stdout.flush

private def stty (arguments : Array String) : IO String :=
  IO.Process.run {
    cmd := "stty"
    args := #["-F", "/dev/tty"] ++ arguments
  }

def dimensions : IO Dimensions := do
  let output ← stty #["size"]
  let fields := output.trimAscii.toString.splitOn " "
  match fields with
  | [rows, columns] =>
      match rows.toNat?, columns.toNat? with
      | some rows, some columns => pure { rows, columns }
      | _, _ => pure { rows := 24, columns := 80 }
  | _ => pure { rows := 24, columns := 80 }

def withTerminal (action : IO.FS.Handle → IO α) : IO α := do
  unless ← (← IO.getStdin).isTty do
    throw (IO.userError "MessageSelf requires an interactive terminal")
  let saved := (← stty #["-g"]).trimAscii.toString
  let tty ← IO.FS.Handle.mk "/dev/tty" .readWrite
  discard <| stty #["raw", "-echo", "min", "0", "time", "5"]
  write (Ansi.enterScreen ++ Ansi.enableMouse)
  try
    action tty
  finally
    write (Ansi.disableMouse ++ Ansi.showCursor ++ Ansi.leaveScreen)
    discard <| stty #[saved]

private def readByte (tty : IO.FS.Handle) : IO (Option UInt8) := do
  let bytes ← tty.read 1
  pure bytes[0]?

private def utf8Size (first : UInt8) : Nat :=
  let byte := first.toNat
  if byte < 0x80 then 1
  else if byte < 0xE0 then 2
  else if byte < 0xF0 then 3
  else 4

private def readText (tty : IO.FS.Handle) (first : UInt8) : IO (Option String) := do
  let mut bytes := ByteArray.empty.push first
  for _ in [1:utf8Size first] do
    match ← readByte tty with
    | some byte => bytes := bytes.push byte
    | none => return none
  pure (String.fromUTF8? bytes)

private partial def readMouse
    (tty : IO.FS.Handle) (fieldsRev : List Nat := []) (current : Nat := 0) : IO Key := do
  match ← readByte tty with
  | some byte =>
      let value := byte.toNat
      if 48 ≤ value && value ≤ 57 then
        readMouse tty fieldsRev (current * 10 + value - 48)
      else if value == 59 then
        readMouse tty (current :: fieldsRev) 0
      else if value == 77 then
        match (current :: fieldsRev).reverse with
        | [0, column, row] => pure (.mouseDown row column)
        | [32, column, row] => pure (.mouseDrag row column)
        | [64, _, _] => pure .wheelUp
        | [65, _, _] => pure .wheelDown
        | _ => pure .idle
      else if value == 109 then
        pure .mouseUp
      else
        pure .idle
  | none => pure .idle

private def readEscape (tty : IO.FS.Handle) : IO Key := do
  match ← readByte tty with
  | some 91 =>
      match ← readByte tty with
      | some 65 => pure .scrollUp
      | some 66 => pure .scrollDown
      | some 53 =>
          discard <| readByte tty
          pure .pageUp
      | some 54 =>
          discard <| readByte tty
          pure .pageDown
      | some 60 => readMouse tty
      | _ => pure .idle
  | _ => pure .idle

def readKey (tty : IO.FS.Handle) : IO Key := do
  match ← readByte tty with
  | none => pure .idle
  | some 3 => pure .quit
  | some 8 | some 127 => pure .backspace
  | some 13 | some 10 => pure .enter
  | some 21 => pure .clearInput
  | some 27 => readEscape tty
  | some byte =>
      if byte < 32 then pure .idle
      else
        match ← readText tty byte with
        | some text => pure (.text text)
        | none => pure .idle

private def outerWidth (size : Dimensions) : Nat :=
  max 20 (size.columns - 1)

def viewportHeight (size : Dimensions) : Nat :=
  max 1 (size.rows - 9)

def isScrollbarHit (size : Dimensions) (row column : Nat) : Bool :=
  let height := viewportHeight size
  outerWidth size ≤ column && 7 ≤ row && row < height + 7

private def entryLines (width : Nat) (entry : Entry) : List String :=
  let innerWidth := width - 4
  let label := clip (width - 6) s!"◆ YOU · {entry.prettyTime}"
  let top := "╭─ " ++ Ansi.amber label ++ " " ++
    repeatText (width - label.length - 5) "─" ++ "╮"
  let body := (wrapText innerWidth entry.message).map fun line =>
    Ansi.dim "│" ++ " " ++ padRight innerWidth line ++ " " ++ Ansi.dim "│"
  top :: body ++ [Ansi.dim (border width "╰" "─" "╯")]

private structure DayGroup where
  day : String
  entriesRev : List Entry

private structure HistoryLine where
  text : String
  day : Option String := none

private def dayGroups (entries : Array Entry) : List DayGroup :=
  let groupsRev := entries.toList.foldl (init := ([] : List DayGroup)) fun groups entry =>
    match groups with
    | group :: rest =>
        if group.day == entry.day then
          { group with entriesRev := entry :: group.entriesRev } :: rest
        else
          { day := entry.day, entriesRev := [entry] } :: groups
    | [] => [{ day := entry.day, entriesRev := [entry] }]
  groupsRev.reverse.map fun group =>
    { group with entriesRev := group.entriesRev.reverse }

private def dayHeading
    (width : Nat) (day today : String) (count : Nat) (expanded : Bool) : String :=
  let marker := if expanded then "▼" else "▶"
  let date := if day == today then s!"Today · {prettyDay day}" else prettyDay day
  let label := s!"{marker} {date} · {count} {if count == 1 then "message" else "messages"}"
  " " ++ Ansi.violet marker ++ " " ++ Ansi.cyan (label.drop 2 |>.toString) ++
    repeatText (width - label.length - 1) " "

private def historyLines
    (size : Dimensions) (entries : Array Entry)
    (today : String) (expandedDays : Array String) : Array HistoryLine :=
  let width := outerWidth size - 2
  if entries.isEmpty then
    let message := "  ◆ No messages yet."
    #[{ text := "  " ++ Ansi.violet "◆" ++ " " ++ Ansi.dim "No messages yet." ++
      repeatText (width - message.length) " " }]
  else
    (dayGroups entries).foldl (init := #[]) fun lines group =>
      let expanded := expandedDays.contains group.day
      let heading := dayHeading width group.day today group.entriesRev.length expanded
      let heading := { text := heading, day := some group.day }
      let blank := { text := repeatText width " " }
      let lines := if lines.isEmpty then #[heading] else lines ++ #[blank, heading]
      if expanded then
        group.entriesRev.foldl (init := lines) fun lines entry =>
          lines ++ #[blank] ++ ((entryLines width entry).toArray.map fun text =>
            ({ text } : HistoryLine))
      else
        lines

def maxScroll
    (size : Dimensions) (entries : Array Entry)
    (today : String) (expandedDays : Array String) : Nat :=
  (historyLines size entries today expandedDays).size - viewportHeight size

def scrollAtScrollbarRow
    (size : Dimensions) (entries : Array Entry)
    (today : String) (expandedDays : Array String) (screenRow : Nat) : Nat :=
  let limit := maxScroll size entries today expandedDays
  let height := viewportHeight size
  if limit == 0 || height ≤ 1 then
    0
  else
    let trackRow := min (height - 1) (screenRow - 7)
    limit - (trackRow * limit / (height - 1))

def dayAtRow
    (size : Dimensions) (entries : Array Entry)
    (today : String) (expandedDays : Array String)
    (scroll screenRow : Nat) : Option String :=
  let lines := historyLines size entries today expandedDays
  let height := viewportHeight size
  if screenRow < 7 || height + 7 ≤ screenRow then
    none
  else
    let scroll := min scroll (lines.size - height)
    let start := lines.size - height - scroll
    (lines[start + screenRow - 7]?).bind (·.day)

private def scrollbar
    (height total start row : Nat) : String :=
  if total ≤ height then " "
  else
    let thumbSize := max 1 (height * height / total)
    let maxStart := total - height
    let thumbTop := start * (height - thumbSize) / maxStart
    if thumbTop ≤ row && row < thumbTop + thumbSize then
      Ansi.violet "█"
    else
      Ansi.dim "│"

private def banner (size : Dimensions) (path : System.FilePath) (count : Nat) : List String :=
  let width := outerWidth size
  let innerWidth := width - 2
  let brand := " ◆ MESSAGE SELF "
  let mode := "LOCAL JOURNAL"
  let status := " ● SAVED "
  let titleWidth := brand.length + 2 + mode.length + 3 + status.length
  let title :=
    Ansi.styled "1;30;48;5;81" brand ++ "  " ++
    Ansi.dim mode ++ "   " ++ Ansi.styled "1;30;48;5;114" status ++
    repeatText (innerWidth - titleWidth) " "
  [
    Ansi.violet (border width "╭" "─" "╮"),
    Ansi.violet "│" ++ title ++ Ansi.violet "│",
    Ansi.violet "│" ++ Ansi.dim (padRight innerWidth s!" {path} • {count} saved messages") ++ Ansi.violet "│",
    Ansi.violet (border width "╰" "─" "╯"),
    Ansi.dim "  Click days • Drag scrollbar • Wheel/↑/↓ scroll • Ctrl-C quits",
    ""
  ]

private def composer (size : Dimensions) (input : String) : List String × Nat :=
  let width := outerWidth size
  let innerWidth := width - 2
  let label := "Message  YOURSELF"
  let top := Ansi.violet "╭─" ++ " " ++ Ansi.amber "Message" ++ "  " ++
    Ansi.dim "YOURSELF" ++ " " ++
    Ansi.violet (repeatText (width - label.length - 4) "─" ++ "╮")
  let capacity := innerWidth - 3
  let visibleInput := takeRight capacity input
  let body := Ansi.violet "│" ++ " " ++ Ansi.amber "❯" ++ " " ++
    padRight capacity visibleInput ++ Ansi.violet "│"
  let bottom := Ansi.violet (border width "╰" "─" "╯")
  ([top, body, bottom], 5 + visibleInput.length)

def render
    (size : Dimensions)
    (path : System.FilePath)
    (entries : Array Entry)
    (today : String)
    (expandedDays : Array String)
    (input : String)
    (scroll : Nat) : IO Unit := do
  let allHistory := historyLines size entries today expandedDays
  let height := viewportHeight size
  let scroll := min scroll (allHistory.size - height)
  let maxStart := allHistory.size - height
  let start := maxStart - scroll
  let width := outerWidth size
  let cardWidth := width - 2
  let visible := (List.range height).map fun row =>
    let line := (allHistory[start + row]?).map (·.text) |>.getD (repeatText cardWidth " ")
    line ++ " " ++ scrollbar height allHistory.size start row
  let (composerLines, cursorColumn) := composer size input
  let lines := banner size path entries.size ++ visible ++ composerLines
  let cursorRow := size.rows - 1
  write <| Ansi.hideCursor ++ Ansi.home ++ String.intercalate "\r\n" lines ++
    s!"\x1b[{cursorRow};{cursorColumn}H" ++ Ansi.showCursor

end MessageSelf.Terminal
