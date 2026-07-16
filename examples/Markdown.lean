import Std

/-!
A small, dependency-free terminal Markdown renderer.

The implementation follows a three-stage pipeline:

  text → block syntax → terminal layout

Keeping parsing, inline markup, and presentation separate makes the supported
Markdown subset explicit and keeps terminal concerns out of the parser.
-/

namespace Markdown

structure Theme where
  color : Bool

namespace Paint

private def styled (theme : Theme) (code text : String) : String :=
  if theme.color then s!"\x1b[{code}m{text}\x1b[0m" else text

def heading (theme : Theme) (level : Nat) (text : String) : String :=
  styled theme (if level ≤ 2 then "1;36" else "1;34") text

def code (theme : Theme) (text : String) : String := styled theme "38;5;214" text
def muted (theme : Theme) (text : String) : String := styled theme "2" text
def quote (theme : Theme) (text : String) : String := styled theme "38;5;108" text
def bullet (theme : Theme) (text : String) : String := styled theme "33" text
def success (theme : Theme) (text : String) : String := styled theme "32" text

def combined (theme : Theme) (codes : List String) (text : String) : String :=
  if codes.isEmpty then text else styled theme (String.intercalate ";" codes) text

end Paint

/-! ## Text utilities -/

private def safeChar (char : Char) : Char :=
  -- Prevent document content from injecting ANSI escape sequences.
  if char == '\x1b' then '␛' else char

private def safeString (text : String) : String :=
  String.ofList (text.toList.map safeChar)

private def startsWith (needle input : List Char) : Bool :=
  match needle, input with
  | [], _ => true
  | _, [] => false
  | expected :: needle, actual :: input =>
      expected == actual && startsWith needle input

private def stripPrefix? (needle input : List Char) : Option (List Char) :=
  if startsWith needle input then some (input.drop needle.length) else none

private def splitAt (needle input : List Char) : Option (List Char × List Char) :=
  let rec go (before : List Char) : List Char → Option (List Char × List Char)
    | [] => none
    | char :: rest =>
        if startsWith needle (char :: rest) then
          some (before.reverse, (char :: rest).drop needle.length)
        else
          go (char :: before) rest
  if needle.isEmpty then none else go [] input

private def normalizeLine (line : String) : String :=
  match line.toList.reverse with
  | '\r' :: rest => String.ofList rest.reverse
  | _ => line

private def repeatText (count : Nat) (text : String) : String :=
  String.join (List.replicate count text)

/-! ## Inline markup -/

inductive Inline where
  | text (value : String)
  | strong (children : List Inline)
  | emphasis (children : List Inline)
  | code (value : String)
  | link (label : List Inline) (url : String)
  | image (alt url : String)

private structure Cursor where
  rest : List Char

private def Cursor.drop (cursor : Cursor) (count : Nat) : Cursor :=
  { rest := cursor.rest.drop count }

private def Cursor.at (rest : List Char) : Cursor :=
  { rest }

private def flushText (textRev : List Char) (nodesRev : List Inline) : List Inline :=
  if textRev.isEmpty then nodesRev
  else .text (safeString (String.ofList textRev.reverse)) :: nodesRev

private def linkParts? (input : List Char) : Option (List Char × List Char × List Char) := do
  let (label, afterLabel) ← splitAt [']', '('] input
  let (url, afterUrl) ← splitAt [')'] afterLabel
  pure (label, url, afterUrl)

/-
`parseInlineFrom` is an executable parser, so it is intentionally `partial`.
Every recursive branch consumes at least one character, while recursive parsing
of a delimited span starts after its opening delimiter. The progress invariant
is local and explicit even though Lean cannot derive it through delimiter search.
-/
private partial def parseInlineFrom
    (closing : Option (List Char))
    (cursor : Cursor)
    (textRev : List Char := [])
    (nodesRev : List Inline := []) : List Inline × Cursor :=
  let matchedClosing := closing.filter (startsWith · cursor.rest)
  match matchedClosing with
  | some marker => (flushText textRev nodesRev |>.reverse, cursor.drop marker.length)
  | none =>
    match cursor.rest with
    | [] => (flushText textRev nodesRev |>.reverse, cursor)
    | '\\' :: char :: _ =>
        parseInlineFrom closing (cursor.drop 2) (safeChar char :: textRev) nodesRev
    | '!' :: '[' :: rest =>
        match linkParts? rest with
        | some (alt, url, after) =>
            let nodesRev := flushText textRev nodesRev
            let node := .image (safeString (String.ofList alt)) (safeString (String.ofList url))
            parseInlineFrom closing (Cursor.at after) [] (node :: nodesRev)
        | none => parseInlineFrom closing (cursor.drop 1) ('!' :: textRev) nodesRev
    | '[' :: rest =>
        match linkParts? rest with
        | some (label, url, after) =>
            let nodesRev := flushText textRev nodesRev
            let label := (parseInlineFrom none { rest := label }).1
            let node := .link label (safeString (String.ofList url))
            parseInlineFrom closing (Cursor.at after) [] (node :: nodesRev)
        | none => parseInlineFrom closing (cursor.drop 1) ('[' :: textRev) nodesRev
    | '`' :: rest =>
        match splitAt ['`'] rest with
        | some (body, after) =>
            let nodesRev := flushText textRev nodesRev
            let node := .code (safeString (String.ofList body))
            parseInlineFrom closing (Cursor.at after) [] (node :: nodesRev)
        | none => parseInlineFrom closing (cursor.drop 1) ('`' :: textRev) nodesRev
    | '*' :: '*' :: rest =>
        if (splitAt ['*', '*'] rest).isSome then
          let nodesRev := flushText textRev nodesRev
          let (children, after) := parseInlineFrom (some ['*', '*']) (cursor.drop 2)
          parseInlineFrom closing after [] (.strong children :: nodesRev)
        else
          parseInlineFrom closing (cursor.drop 1) ('*' :: textRev) nodesRev
    | '_' :: '_' :: rest =>
        if (splitAt ['_', '_'] rest).isSome then
          let nodesRev := flushText textRev nodesRev
          let (children, after) := parseInlineFrom (some ['_', '_']) (cursor.drop 2)
          parseInlineFrom closing after [] (.strong children :: nodesRev)
        else
          parseInlineFrom closing (cursor.drop 1) ('_' :: textRev) nodesRev
    | '*' :: rest =>
        if (splitAt ['*'] rest).isSome then
          let nodesRev := flushText textRev nodesRev
          let (children, after) := parseInlineFrom (some ['*']) (cursor.drop 1)
          parseInlineFrom closing after [] (.emphasis children :: nodesRev)
        else
          parseInlineFrom closing (cursor.drop 1) ('*' :: textRev) nodesRev
    | '_' :: rest =>
        if (splitAt ['_'] rest).isSome then
          let nodesRev := flushText textRev nodesRev
          let (children, after) := parseInlineFrom (some ['_']) (cursor.drop 1)
          parseInlineFrom closing after [] (.emphasis children :: nodesRev)
        else
          parseInlineFrom closing (cursor.drop 1) ('_' :: textRev) nodesRev
    | char :: _ =>
        parseInlineFrom closing (cursor.drop 1) (safeChar char :: textRev) nodesRev

private def parseInline (text : String) : List Inline :=
  (parseInlineFrom none { rest := text.toList }).1

private def renderInlinesWith (theme : Theme) (styles : List String) : List Inline → String
  | [] => ""
  | node :: rest =>
      let rendered :=
        match node with
        | .text value => Paint.combined theme styles value
        | .strong children => renderInlinesWith theme ("1" :: styles) children
        | .emphasis children => renderInlinesWith theme ("3" :: styles) children
        | .code value => Paint.combined theme ("38;5;214" :: styles) value
        | .link label url =>
            renderInlinesWith theme ("4" :: "36" :: styles) label ++
              Paint.combined theme ("2" :: styles) s!" ({url})"
        | .image alt url =>
            Paint.combined theme ("2" :: styles) "image: " ++
              Paint.combined theme ("4" :: "36" :: styles) alt ++
              Paint.combined theme ("2" :: styles) s!" ({url})"
      rendered ++ renderInlinesWith theme styles rest

private def renderInlines (theme : Theme) (inlines : List Inline) : String :=
  renderInlinesWith theme [] inlines

private def inlineWidth : List Inline → Nat
  | [] => 0
  | node :: rest =>
      let width :=
        match node with
        | .text value | .code value => value.length
        | .strong children | .emphasis children => inlineWidth children
        | .link label url => inlineWidth label + url.length + 3
        | .image alt url => alt.length + url.length + 10
      width + inlineWidth rest

/-! ## Block syntax -/

inductive ListMarker where
  | bullet
  | ordered (number : String)
  | task (checked : Bool)

inductive Block where
  | heading (level : Nat) (content : List Inline)
  | paragraph (content : List Inline)
  | quote (content : List Inline)
  | listItem (marker : ListMarker) (content : List Inline)
  | rule
  | indentedCode (text : String)
  | fencedCode (language : String) (lines : List String)

private def heading? (line : String) : Option (Nat × String) :=
  let chars := line.toList
  let marks := chars.takeWhile (· == '#')
  let level := marks.length
  match chars.drop level with
  | ' ' :: body =>
      if level > 0 && level ≤ 6 then some (level, String.ofList body) else none
  | _ => none

private def orderedItem? (line : String) : Option (String × String) :=
  let chars := line.toList
  let digits := chars.takeWhile Char.isDigit
  if digits.isEmpty then none
  else
    match chars.drop digits.length with
    | '.' :: ' ' :: body => some (String.ofList digits, String.ofList body)
    | _ => none

private def rule? (line : String) : Bool :=
  let chars := line.trimAscii.toString.toList
  chars.length ≥ 3 &&
    (chars.all (· == '-') || chars.all (· == '*') || chars.all (· == '_'))

private def taskItem (body : List Char) : ListMarker × String :=
  match stripPrefix? ['[', ' ', ']', ' '] body with
  | some text => (.task false, String.ofList text)
  | none =>
      match stripPrefix? ['[', 'x', ']', ' '] body <|>
          stripPrefix? ['[', 'X', ']', ' '] body with
      | some text => (.task true, String.ofList text)
      | none => (.bullet, String.ofList body)

private def classifyLine (line : String) : Block :=
  let chars := line.toList
  match heading? line with
  | some (level, text) => .heading level (parseInline text)
  | none =>
      if rule? line then .rule
      else
        match stripPrefix? ['>', ' '] chars with
        | some text => .quote (parseInline (String.ofList text))
        | none =>
            match stripPrefix? ['-', ' '] chars <|>
                stripPrefix? ['*', ' '] chars <|>
                stripPrefix? ['+', ' '] chars with
            | some body =>
                let (marker, text) := taskItem body
                .listItem marker (parseInline text)
            | none =>
                match orderedItem? line with
                | some (number, text) => .listItem (.ordered number) (parseInline text)
                | none =>
                    match stripPrefix? [' ', ' ', ' ', ' '] chars with
                    | some text => .indentedCode (String.ofList text)
                    | none => .paragraph (parseInline line)

private def openingFence? (line : String) : Option (Nat × String) :=
  let chars := line.toList
  let ticks := chars.takeWhile (· == '`')
  if ticks.length < 3 then none
  else
    let language := String.ofList (chars.drop ticks.length) |>.trimAscii.toString
    some (ticks.length, language)

private def closesFence (width : Nat) (line : String) : Bool :=
  let chars := line.trimAscii.toString.toList
  chars.length ≥ width && chars.all (· == '`')

private structure OpenFence where
  width : Nat
  language : String
  linesRev : List String := []

private structure ParserState where
  blocksRev : List Block := []
  fence : Option OpenFence := none

private def ParserState.push (state : ParserState) (block : Block) : ParserState :=
  { state with blocksRev := block :: state.blocksRev }

private def parseLine (state : ParserState) (rawLine : String) : ParserState :=
  let line := normalizeLine rawLine
  match state.fence with
  | some fence =>
      if closesFence fence.width line then
        { blocksRev := .fencedCode fence.language fence.linesRev.reverse :: state.blocksRev }
      else
        { state with fence := some { fence with linesRev := line :: fence.linesRev } }
  | none =>
      match openingFence? line with
      | some (width, language) => { state with fence := some { width, language } }
      | none => state.push (classifyLine line)

private def ParserState.finish (state : ParserState) : List Block :=
  let blocksRev :=
    match state.fence with
    | none => state.blocksRev
    | some fence => .fencedCode fence.language fence.linesRev.reverse :: state.blocksRev
  blocksRev.reverse

private def sourceLines (markdown : String) : List String :=
  match (markdown.splitOn "\n").reverse with
  | "" :: rest => rest.reverse
  | lines => lines.reverse

private def parseBlocks (markdown : String) : List Block :=
  (sourceLines markdown).foldl parseLine {} |>.finish

/-! ## Terminal layout -/

private def renderHeading (theme : Theme) (level : Nat) (content : List Inline) : List String :=
  let title := renderInlines theme content
  if level ≤ 2 then
    let rule := repeatText ((inlineWidth content).min 72) (if level == 1 then "━" else "─")
    [Paint.heading theme level title, Paint.muted theme rule]
  else
    [Paint.heading theme level (repeatText (level - 2) "›" ++ " " ++ title)]

private def renderListItem (theme : Theme) (marker : ListMarker) (content : List Inline) : String :=
  let markerText :=
    match marker with
    | .bullet => Paint.bullet theme "  • "
    | .ordered number => Paint.bullet theme s!"  {number}. "
    | .task false => Paint.muted theme "  ☐ "
    | .task true => Paint.success theme "  ☑ "
  markerText ++ renderInlines theme content

private def renderCodeBlock (theme : Theme) (language : String) (lines : List String) : List String :=
  let label := safeString (if language.isEmpty then "code" else language)
  let lines := lines.map safeString
  let longest := lines.foldl (fun width line => max width line.length) 0
  let width := max 38 (max longest (label.length + 3))
  let top :=
    "  ┌─ " ++ label ++ " " ++ repeatText (width - label.length - 1) "─" ++ "┐"
  let rows := lines.map fun line =>
    Paint.muted theme "  │ " ++ Paint.code theme line ++
      Paint.muted theme (repeatText (width - line.length) " " ++ " │")
  let bottom := "  └" ++ repeatText (width + 2) "─" ++ "┘"
  Paint.muted theme top :: rows ++ [Paint.muted theme bottom]

private def renderBlock (theme : Theme) : Block → List String
  | .heading level content => renderHeading theme level content
  | .paragraph content => [renderInlines theme content]
  | .quote content => [Paint.quote theme "  │ " ++ renderInlines theme content]
  | .listItem marker content => [renderListItem theme marker content]
  | .rule => [Paint.muted theme (repeatText 58 "─")]
  | .indentedCode text => [Paint.code theme ("  " ++ safeString text)]
  | .fencedCode language lines => renderCodeBlock theme language lines

def render (theme : Theme) (markdown : String) : String :=
  parseBlocks markdown
    |>.flatMap (renderBlock theme)
    |> String.intercalate "\n"

/-! ## Command-line interface -/

private structure Options where
  path : Option String := none
  color : Option Bool := none
  help : Bool := false

private def parseArgs (args : List String) : Except String Options :=
  let rec go (options : Options) : List String → Except String Options
    | [] => pure options
    | arg :: rest => do
        let options ←
          match arg with
          | "--color" => pure { options with color := some true }
          | "--no-color" => pure { options with color := some false }
          | "-h" | "--help" => pure { options with help := true }
          | "-" =>
              match options.path with
              | none => pure { options with path := some "-" }
              | some _ => throw "expected at most one input file"
          | _ =>
              if arg.startsWith "-" then throw s!"unknown option: {arg}"
              else
                match options.path with
                | none => pure { options with path := some arg }
                | some _ => throw "expected at most one input file"
        go options rest
  go {} args

private def usage : String :=
  "Usage: markdown [OPTIONS] [FILE]\n\n" ++
  "Render a useful subset of Markdown to the terminal. Reads standard input " ++
  "when\nFILE is omitted or is '-'.\n\n" ++
  "Options:\n" ++
  "  --color       Force ANSI styling\n" ++
  "  --no-color    Disable ANSI styling\n" ++
  "  -h, --help    Show this help\n\n" ++
  "The NO_COLOR environment variable also disables styling.\n"

private def readMarkdown (path : Option String) : IO String := do
  match path with
  | none | some "-" => (← IO.getStdin).readToEnd
  | some filename => IO.FS.readFile filename

private def resolveColor : Option Bool → IO Bool
  | some enabled => pure enabled
  | none => return (← IO.getEnv "NO_COLOR").isNone

def run (args : List String) : IO UInt32 := do
  match parseArgs args with
  | .error message =>
      IO.eprintln s!"markdown: {message}\nTry 'markdown --help' for usage."
      pure 2
  | .ok options =>
      if options.help then
        IO.print usage
        pure 0
      else
        try
          let markdown ← readMarkdown options.path
          let color ← resolveColor options.color
          IO.println (render { color } markdown)
          pure 0
        catch error =>
          IO.eprintln s!"markdown: {error}"
          pure 1

end Markdown

def main (args : List String) : IO UInt32 :=
  Markdown.run args
