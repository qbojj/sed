open Result
open Regex

type address =
  | Line of int
  | Steps of int * int
  | Regex of regex
  | Last

type address_range = 
  | Always
  | Single of address
  | Range of address * address

type replace_flags =
  | CaseInsensitive
  | MultiMatch

type replace_info =
  { src: regex; dst: regex_replacement; flags: replace_flags list }

type command =
  | Label of string
  | PrintLineNo of address option
  | Append of address option * string
  | Insert of address option * string
  | QuitAutoprint of address option
  | Quit of address option
  | AppendFile of address option * string
  | Block of address_range * command list
  | Branch of address_range * string
  | ReplaceText of address_range * string
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
type position = { row: int; column: int }
type 'a result = ('a, parse_error) Result.t

type non_blockmatched_command =
  | BeginBlock of address_range * position
  | EndBlock of position
  | Label of string
  | PrintLineNo of address option
  | Append of address option * string
  | Insert of address option * string
  | QuitAutoprint of address option
  | Quit of address option
  | AppendFile of address option * string
  | Branch of address_range * string
  | ReplaceText of address_range * string
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

let make_err { row; column } msg = { row; column; msg }
let err pos msg = Error (make_err pos msg)

let (let*) = Result.bind
let return: 'a -> 'a result = Result.ok

type pos_str = (position * char) list

let pos_str_of_string s =
  let rec impl ({ row; column } as pos) i acc =
    if i = String.length s then List.rev acc
    else
      let c = String.get s i in
      let np = match c with
        | '\n' -> { row=row + 1; column=1; }
        | _ -> { row; column=column+1 } in
      impl np (i+1) ((pos, c) :: acc) in
  impl { row=1; column=1 } 0 []

let skipWhites = List.drop_while (fun (_, c) -> match c with ' ' | '\t' -> true | _ -> false)

let splitAtNonescapedNewlines (code: pos_str): pos_str list result =
  let rec impl acc lineacc = function
    | [] -> return (List.rev ((List.rev lineacc) :: acc))
    | (pos, c) :: rest ->
      match c with
      | '\n' -> impl (List.rev lineacc :: acc) [] rest
      | '\\' -> (
        match rest with
        | (_, '\n') as v :: rest -> impl acc (v :: lineacc) rest
        | _ -> impl acc ((pos, c) :: lineacc) rest
      )
      | _ -> impl acc ((pos, c) :: lineacc) rest in
  impl [] [] code

let mapM f l =
  let rec impl acc = function
    | [] -> return (List.rev acc)
    | x :: xs -> let* v = f x in impl (v :: acc) xs in
  impl [] l

(*
number
first~step
$
/regexp/
\cregexpc (c is any character)
*)

type echar =
  | Escaped of char
  | Unescaped of char

(* if first char is ^ -> negation *)
(* the second char can be ] *)
(* cannot escape values inside the [] *)
(* '-' can be at the front or at the back (right near [ or ])*)
(* the set cannot be empty *)
let parseCharSet (pos: position) (code: (position * echar) list): ((position * echar) list * regex) result =
  let* (first, rest) = match code with | [] -> err pos "empty charset" | first :: rest -> return (first, rest) in
  let module CSet = Set.Make(Char) in
  let* (negated, content, rest) = match first with 
    | (_, Unescaped '^') -> 
      begin match rest with
      | [] -> err pos "empty negated charset"
      | (_, Unescaped ']') :: rest -> return (true, CSet.singleton ']', rest)
      | _ -> return (true, CSet.empty, rest)
      end
    | _ -> return (false, CSet.empty,rest) in
  let rec find_end acc = function
    | [] -> err pos "unterminated charset"
    | (pos, Escaped c) :: rest -> find_end acc ((pos, Unescaped '\\') :: (pos, Unescaped c) :: rest)
    | (_, Unescaped ']') :: rest -> return (rest, List.rev acc)
    | (pos, Unescaped c) :: rest -> find_end ((pos, c) :: acc) rest in
  let* (rest, chars) = find_end [] rest in
  (* parse chars (with '-' support) *)
  let rec impl acc = function
    | [] -> return (RLit (fun c -> (CSet.mem c acc) <> negated))
    | (_, '-') :: rest -> impl (CSet.add '-' acc) rest
    | (_, c1) :: (pos, '-') :: (_, c2) :: rest ->
      let c1 = Char.code c1 in
      let c2 = Char.code c2 in
      if c1 > c2 then err pos "invalid range"
      else
        let rec add_range acc c1 c2 =
          if c1 > c2 then acc
          else add_range (CSet.add (Char.chr c1) acc) (c1 + 1) c2 in
        impl (add_range acc c1 c2) rest
    | (_, c) :: rest -> impl (CSet.add c acc) rest in
  let* reg = impl content chars in
  return (rest, reg)

let parseRegex (code: pos_str) (endc: char): (pos_str * regex) result =
  (* find end of the regex (make sure endc is not escaped) *)
  let rec impl pos reg_str = function
    | [] -> err pos "unterminated regex"
    | (pos, '\\') :: (_, c) :: rest -> impl pos ((pos, Escaped c) :: reg_str) rest
    | (_, c) :: rest when c = endc -> return (rest, List.rev reg_str)
    | (pos, c) :: rest -> impl pos ((pos, Unescaped c) :: reg_str) rest in
  let* (after_regex, reg_str) = impl { row=(-1); column=(-1) } [] code in
  let position = code |> List.hd |> fst in
  let rec impl acc depth = function
    | [] as rest when depth = 0 -> return (rest, RList (List.rev acc))
    | (_, Unescaped ')') :: rest when depth > 0 -> return (rest, RList (List.rev acc))
    | [] -> err position "unterminated group"
    | (pos, Unescaped ')') :: _ -> err pos "unmatched )"
    | (_, Escaped c) :: rest -> impl (RLit ((=) c) :: acc) depth rest
    | (_, Unescaped '(') :: rest ->
      let* (rest, reg) = impl [] (depth + 1) rest in
      impl (reg :: acc) depth rest
    | (_, Unescaped '|') :: rest ->
      let* (rest, reg) = impl [] depth rest in
      return (rest, ROr (RList (List.rev acc), reg))
    | (pos, Unescaped '*') :: _ when List.is_empty acc -> err pos "nothing to star"
    | (_, Unescaped '*') :: rest -> impl (RStar (List.hd acc) :: List.tl acc) depth rest
    | (pos, Unescaped '?') :: _ when List.is_empty acc -> err pos "nothing to opt"
    | (_, Unescaped '?') :: rest -> impl (ROpt (List.hd acc) :: List.tl acc) depth rest
    | (pos, Unescaped '+') :: _ when List.is_empty acc -> err pos "nothing to plus"
    | (_, Unescaped '+') :: rest -> impl (RStar (List.hd acc) :: acc) depth rest
    | (_, Unescaped '.') :: rest -> impl (RLit (fun _ -> true) :: acc) depth rest
    | (_, Unescaped '^') :: rest -> impl (RBeg :: acc) depth rest
    | (_, Unescaped '$') :: rest -> impl (REnd :: acc) depth rest
    | (pos, Unescaped '[') :: rest ->
      let* (rest, reg) = parseCharSet pos rest in
      impl (reg :: acc) depth rest
    | (_, Unescaped c) :: rest -> impl (RLit ((=) c) :: acc) depth rest in
  let* (_, reg) = impl [] 0 reg_str in
  return (after_regex, reg)

let parseToString code endc =
  let rec impl pos acc = function
    | [] -> err pos "the to-string is not terminated"
    | (_, '\\') :: (pos, c) :: rest -> impl pos (c :: acc) rest
    | (_, c) :: rest when c = endc -> return (rest, List.rev acc)
    | (pos, c) :: rest -> impl pos (c :: acc) rest in
  let* (rest, r) = impl {row=(-1); column=(-1)} [] code in
  return (rest, r |> List.to_seq |> String.of_seq)

let digits = "0123456789"

let parseNumber (code: pos_str): pos_str * int =
  let rec impl acc rest = match rest with
    | [] -> (rest, acc)
    | (_, c) :: rest when String.contains digits c -> 
      impl (acc * 10 + Char.code c - Char.code '0') rest
    | _ -> (rest, acc) in
  impl 0 code

let toString code = code |> skipWhites |> List.map (fun (_, c) -> c) |> List.to_seq |> String.of_seq

let parseAddress (code: pos_str): (pos_str * address option) result =
  let code = skipWhites code in
  match code with
  | [] -> return (code, None)
  | (_, '$') :: rest -> return (rest, Some Last)
  | (_, '/') :: rest -> let* (rest, reg) = parseRegex rest '/' in return (rest, Some (Regex reg))
  | (_, '\\') :: (_, c) :: rest -> let* (rest, reg) = parseRegex rest c in return (rest, Some (Regex reg))
  | (_, c) :: _ when String.contains digits c ->
    let (rest, v) = parseNumber code in
    print_string "parseAddress\n";
    Printf.printf "%s\n" (rest |> toString);
    begin match rest with
    | (_, '~') :: ((_, c) :: _) as rest when String.contains digits c ->
      let (rest, step) = parseNumber rest in
      return (rest, Some (Steps (v, step)))
    | (pos, '~') :: _ -> err pos "invalid step"
    | _ -> return (rest, Some (Line v))
    end
  | _ -> return (code, None)

let parseAddresses (code: pos_str): (pos_str * address_range) result =
  let* (rest, ad1) = parseAddress code in
  match ad1 with
  | None -> return (rest, Always)
  | Some ad1 -> 
    match code with
    | [] -> return (rest, Single ad1)
    | (_, ',') :: rest ->
      let* (rest, ad2) = parseAddress rest in
      begin match ad2 with
      | None -> err (List.hd rest |> fst) "invalid address"
      | Some ad2 -> return (rest, Range (ad1, ad2))
      end
    | _ -> return (rest, Single ad1)

let parseToEnd code =
  let rec impl acc = function
    | [] -> return ([], List.rev acc)
    | (_, ';') :: rest -> return (rest, List.rev acc)
    | (_, '{') as v :: rest -> return (v :: rest, List.rev acc)
    | (_, '}') as v :: rest -> return (v :: rest, List.rev acc)
    | (_, '#') :: _ -> return ([], List.rev acc)
    | v :: rest -> impl (v :: acc) rest in
  impl [] code

let parseLineCommand (code: pos_str): (pos_str * non_blockmatched_command option) result =
  let* (code, addr) = parseAddresses code in
  let simple_addr pos =
    match addr with
    | Always -> return @@ None
    | Single v -> return @@ Some v
    | _ -> err pos "invalid address" in
  let code = skipWhites code in
  match code with
  | [] -> return ([], None)
  | (_, '#') :: _ -> return ([], None)
  | (_, ':') :: rest ->
    let* (rest, name) = parseToEnd rest in
    return (rest, Some (Label (toString name |> String.trim)))
  | (pos, 'a') :: rest -> 
    let* (rest, text) = parseToEnd rest in
    let* simple = simple_addr pos in
    return (rest, Some (Append (simple, toString text |> String.trim)))
  | (pos, 'i') :: rest ->
    let* (rest, text) = parseToEnd rest in
    let* simple = simple_addr pos in
    return (rest, Some (Insert (simple, toString text)))
  | (_, 'c') :: rest ->
    let* (rest, text) = parseToEnd rest in
    return (rest, Some (ReplaceText (addr, toString text)))
  | (pos, 'q') :: rest ->
    let* simple = simple_addr pos in
    return (rest, Some (QuitAutoprint simple))
  | (pos, 'Q') :: rest ->
    let* simple = simple_addr pos in
    return (rest, Some (Quit simple))
  | (pos, 'r') :: rest ->
    let* (rest, file) = parseToEnd rest in
    let* simple = simple_addr pos in
    return (rest, Some (AppendFile (simple, toString file)))
  | (pos, '{') :: rest ->
    return (rest, Some (BeginBlock (addr, pos)))
  | (pos, '}') :: rest ->
    return (rest, Some (EndBlock pos))
  | (pos, '=') :: rest ->
    let* simple = simple_addr pos in
    return (rest, Some (PrintLineNo simple))
  | (_, 'b') :: rest ->
    let* (rest, name) = parseToEnd rest in
    return (rest, Some (Branch (addr, toString name)))
  | (_, 't') :: rest ->
    let* (rest, name) = parseToEnd rest in
    return (rest, Some (CondJmp (addr, toString name)))
  | (pos, 's') :: rest ->
    let rest = skipWhites rest in
    let* (sep, rest) = match rest with | [] -> err pos "no separator" | (_, c) :: rest -> return (c, rest) in
    let* (rest, src) = parseRegex rest sep in
    let* (rest, dst) = parseToString rest sep in
    let* (rest, flags) = parseToEnd rest in
    (* parse flags *)
    let flags =
      flags
      |> List.filter (fun (_, c) -> c <> ' ' && c <> '\t')
      |> List.map (fun (pos, c) -> match c with
        | 'I' -> return CaseInsensitive
        | 'g' -> return MultiMatch
        | _ -> err pos "invalid flag") in
    let* flags = mapM Fun.id flags in
    return (rest, Some (Replace (addr, { src; dst; flags })))
  | (_, 'd') :: rest -> 
    let* (rest, _) = parseToEnd rest in
    return (rest, Some (Delete addr))
  | (_, 'D') :: rest ->
    let* (rest, _) = parseToEnd rest in
    return (rest, Some (DeleteLine addr))
  | (_, 'h') :: rest ->
    let* (rest, _) = parseToEnd rest in
    return (rest, Some (CopyToHold addr))
  | (_, 'H') :: rest ->
    let* (rest, _) = parseToEnd rest in
    return (rest, Some (AppendToHold addr))
  | (_, 'g') :: rest ->
    let* (rest, _) = parseToEnd rest in
    return (rest, Some (CopyFromHold addr))
  | (_, 'G') :: rest ->
    let* (rest, _) = parseToEnd rest in
    return (rest, Some (AppendFromHold addr))
  | (_, 'l') :: rest ->
    let* (rest, _) = parseToEnd rest in
    return (rest, Some (Print addr))
  | (_, 'n') :: rest ->
    let* (rest, _) = parseToEnd rest in
    return (rest, Some (Next addr))
  | (_, 'N') :: rest ->
    let* (rest, _) = parseToEnd rest in
    return (rest, Some (AppendNext addr))
  | (_, 'p') :: rest ->
    let* (rest, _) = parseToEnd rest in
    return (rest, Some (Print addr))
  | (_, 'P') :: rest ->
    let* (rest, _) = parseToEnd rest in
    return (rest, Some (PrintFirst addr))
  | (_, 'w') :: rest ->
    let* (rest, file) = parseToEnd rest in
    return (rest, Some (Write (addr, toString file)))
  | (_, 'x') :: rest ->
    let* (rest, _) = parseToEnd rest in
    return (rest, Some (ExchangeHold addr))
  | (pos, 'y') :: rest ->
    let rest = skipWhites rest in
    let* (rest, sep) = match rest with | [] -> err pos "no separator" | (_, c) :: rest -> return (rest, c) in
    let* (rest, src) = parseToString rest sep in
    let* (rest, dst) = parseToString rest sep in
    if String.length src <> String.length dst then err pos "transliterate strings must have the same length"
    else
      let pairs = List.init (String.length src) (fun i -> (src.[i], dst.[i])) in
      return (rest, Some (Transliterate (addr, pairs)))
  | (pos, c) :: _ ->
    err pos ("invalid command " ^ String.make 1 c) 


let parseLineCommands code =
  let rec impl acc = function
  | [] -> return @@ List.rev acc
  | rest ->
    let* (rest, cmd) = parseLineCommand rest in
    match cmd with
    | None -> impl acc rest
    | Some cmd -> impl (cmd :: acc) rest in
  impl [] code

let simpleEscapes = [
  ('n', '\n');
  ('t', '\t');
  ('r', '\r');
]

let escapeString code =
  let rec impl acc = function
    | [] -> List.rev acc
    | (pos, '\\') :: (_, c) :: rest when List.mem_assoc c simpleEscapes -> impl ((pos, List.assoc c simpleEscapes) :: acc) rest
    | v :: rest -> impl (v :: acc) rest in
  impl [] code

let matchBlocks (xs: non_blockmatched_command list): command list result =
  let rec matchBlock depth pos acc = function
    | [] when depth = 0 -> return (List.rev acc, [])
    | [] -> err pos "unterminated block"
    | (BeginBlock (addr, pos)) :: rest ->
      let* (cmds, rest) = matchBlock (depth + 1) pos [] rest in
      matchBlock depth pos (Block (addr, cmds) :: acc) rest
    | (EndBlock pos) :: _ when depth = 0 -> err pos "unmatched }"
    | (EndBlock _) :: rest -> return (List.rev acc, rest)
    | (Label x) :: rest -> matchBlock depth pos (Label x :: acc) rest
    | (PrintLineNo addr) :: rest -> matchBlock depth pos (PrintLineNo addr :: acc) rest
    | (Append (addr, x)) :: rest -> matchBlock depth pos (Append (addr, x) :: acc) rest
    | (Insert (addr, x)) :: rest -> matchBlock depth pos (Insert (addr, x) :: acc) rest
    | (QuitAutoprint addr) :: rest -> matchBlock depth pos (QuitAutoprint addr :: acc) rest
    | (Quit addr) :: rest -> matchBlock depth pos (Quit addr :: acc) rest
    | (AppendFile (addr, x)) :: rest -> matchBlock depth pos (AppendFile (addr, x) :: acc) rest
    | (Branch (addr, x)) :: rest -> matchBlock depth pos (Branch (addr, x) :: acc) rest
    | (ReplaceText (addr, x)) :: rest -> matchBlock depth pos (ReplaceText (addr, x) :: acc) rest
    | (Delete addr) :: rest -> matchBlock depth pos (Delete addr :: acc) rest
    | (DeleteLine addr) :: rest -> matchBlock depth pos (DeleteLine addr :: acc) rest
    | (CopyToHold addr) :: rest -> matchBlock depth pos (CopyToHold addr :: acc) rest
    | (AppendToHold addr) :: rest -> matchBlock depth pos (AppendToHold addr :: acc) rest
    | (CopyFromHold addr) :: rest -> matchBlock depth pos (CopyFromHold addr :: acc) rest
    | (AppendFromHold addr) :: rest -> matchBlock depth pos (AppendFromHold addr :: acc) rest
    | (Next addr) :: rest -> matchBlock depth pos (Next addr :: acc) rest
    | (AppendNext addr) :: rest -> matchBlock depth pos (AppendNext addr :: acc) rest
    | (Print addr) :: rest -> matchBlock depth pos (Print addr :: acc) rest
    | (PrintFirst addr) :: rest -> matchBlock depth pos (PrintFirst addr :: acc) rest
    | (Replace (addr, x)) :: rest -> matchBlock depth pos (Replace (addr, x) :: acc) rest
    | (CondJmp (addr, x)) :: rest -> matchBlock depth pos (CondJmp (addr, x) :: acc) rest
    | (Write (addr, x)) :: rest -> matchBlock depth pos (Write (addr, x) :: acc) rest
    | (ExchangeHold addr) :: rest -> matchBlock depth pos (ExchangeHold addr :: acc) rest
    | (Transliterate (addr, x)) :: rest -> matchBlock depth pos (Transliterate (addr, x) :: acc) rest in
  let* (cmds, _) = matchBlock 0 {row=(-1); column=(-1)} [] xs in
  return cmds

let parseScript script =
  let code = pos_str_of_string script in
  let* lines = splitAtNonescapedNewlines code in
  let* commands = mapM parseLineCommands @@ List.map escapeString lines in
  matchBlocks @@ List.flatten @@ commands
