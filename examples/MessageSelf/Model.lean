import Lean.Data.Json
import Std.Time.Format
import Std.Time.Zoned

namespace MessageSelf

structure Entry where
  createdAt : String
  message : String
deriving Repr, Lean.ToJson, Lean.FromJson

def Entry.create (message : String) : IO Entry := do
  let createdAt := toString (← Std.Time.PlainDateTime.now)
  pure { createdAt, message }

def currentDay : IO String := do
  pure (toString (← Std.Time.PlainDate.now))

def Entry.day (entry : Entry) : String :=
  (entry.createdAt.take 10).toString

private def monthName : Nat → String
  | 1 => "January"
  | 2 => "February"
  | 3 => "March"
  | 4 => "April"
  | 5 => "May"
  | 6 => "June"
  | 7 => "July"
  | 8 => "August"
  | 9 => "September"
  | 10 => "October"
  | 11 => "November"
  | 12 => "December"
  | _ => "Unknown"

def prettyDay (day : String) : String :=
  match day.splitOn "-" with
  | [year, month, date] =>
      match month.toNat?, date.toNat? with
      | some month, some date => s!"{monthName month} {date}, {year}"
      | _, _ => day
  | _ => day

def Entry.prettyTime (entry : Entry) : String :=
  let time := (entry.createdAt.splitOn "T").getD 1 ""
  match time.splitOn ":" with
  | hour :: minute :: _ =>
      match hour.toNat? with
      | some hour =>
          let clockHour := if hour % 12 == 0 then 12 else hour % 12
          let period := if hour < 12 then "AM" else "PM"
          s!"{clockHour}:{minute} {period}"
      | none => time
  | _ => time

end MessageSelf
