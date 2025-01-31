open Regex

type address =
  | Line of int
  | Steps of int * int
  | Regex of regex
  | Last

type address_range = 
  | Always
  | Single of address * bool
  | Range of address * address * bool

type replace_flags =
  | CaseInsensitive
  | MultiMatch

type replace_info =
  { src: regex; dst: regex_replacement; flags: replace_flags list }

type command =
  | Label of string
  | PrintLineNo of (address * bool) option
  | Append of (address * bool) option * string
  | Insert of (address * bool) option * string
  | QuitAutoprint of (address * bool) option
  | Quit of (address * bool) option
  | AppendFile of (address * bool) option * string
  | Block of address_range * command list
  | Branch of address_range * string
  | ReplaceText of address_range * string
  | PrintEscaped of address_range
  | Delete of address_range
  | DeleteLine of address_range
  | CopyToHold of address_range
  | AppendToHold of address_range
  | CopyFromHold of address_range
  | AppendFromHold of address_range
  | Next of address_range
  | AppendNext of address_range
  | Print of address_range
  | PrintFirst of address_range
  | Replace of address_range * replace_info
  | CondJmp of address_range * string
  | Write of address_range * string
  | ExchangeHold of address_range
  | Transliterate of address_range * (char * char) list

type parse_error = { row: int; column: int; msg: string }

val parseScript : string -> (command list, parse_error) Result.t
