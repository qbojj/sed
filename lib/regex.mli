type regex =
  | RBeg
  | REnd
  | RStar of regex
  | ROpt of regex
  | RLit of (char -> bool)
  | ROr of regex * regex
  | RList of regex list

type regex_replacement = string

type compiled_regex = regex
type match_info = int * int

val compile : regex -> compiled_regex
val compile_insensitive : regex -> compiled_regex

val matches : compiled_regex -> string -> match_info Seq.t
val replace : regex_replacement -> string -> match_info Seq.t -> string
val replace_all : regex_replacement -> string -> match_info Seq.t -> string
