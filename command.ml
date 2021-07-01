(** The type [command] represents a possible player command *)
type command =
  |Quit
  |Score 
  |Help
  |Hint
  |Leaderboard
  |Entry of string

(** Raised when an empty command is parsed *)
exception Empty

(** Parse a string by identifying what command to execute. *)
let parse str = 
  match (String.trim str) with
  |""-> raise Empty
  |"#quit" -> Quit
  |"#score" -> Score
  |"#help" -> Help
  |"#leaderboard" -> Leaderboard
  |"#hint" -> Hint
  |x -> Entry (x)