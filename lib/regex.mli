type regex =
  | RBeg
  | REnd
  | RStar of regex
  | ROpt of regex
  | RLit of (char -> bool)
  | ROr of regex * regex
  | RList of regex list
  | RGroup of regex * int

type regex_replacement_item =
  | Lit of string
  | WholeMatch
  | Group of int

type regex_replacement = regex_replacement_item list
type compiled_regex
type match_info

val parse_replacement : string -> regex_replacement

type regex_config = {
  case_insensitive: bool;
  supports_groups: bool;
}

val compile : regex_config -> regex -> compiled_regex

val matches : compiled_regex -> string -> match_info Seq.t
val replace : regex_replacement -> string -> match_info Seq.t -> string
val replace_all : regex_replacement -> string -> match_info Seq.t -> string
