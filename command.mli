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

(** [parse str] is a command associated with [str]

    Requires: [str] is a valid string
    Raises: Empty if the input is just whitespace
            Invalid if the input is not associated with a command

    @return The command associated with the user's input

    example: [parse "start"] is [Start]
             [parse "begin"] raise Invalid
             [parse "   quit   "] is [Quit]*)
val parse : string -> command
