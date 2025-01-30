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

let compile r = r
let rec compile_insensitive = function
  | RBeg -> RBeg
  | REnd -> REnd
  | RLit f -> RLit (fun c -> (f @@ Char.lowercase_ascii c) || (f @@ Char.uppercase_ascii c))
  | ROpt r -> ROpt (compile_insensitive r)
  | RStar r -> RStar (compile_insensitive r)
  | RList rl -> RList (List.map compile_insensitive rl)
  | ROr (r1, r2) -> ROr (compile_insensitive r1, compile_insensitive r2)

let replace_all rep s matches =
  (* matches are non intersecting and in ascending order *)
  Seq.fold_left (fun (res, offset) (start, end_) ->
    let prefix = String.sub s offset (start - offset) in
    (res ^ prefix ^ rep, end_)
  ) ("", 0) matches
  |> fun (res, offset) -> res ^ String.sub s offset (String.length s - offset)

let replace rep s matches =
  replace_all rep s @@ Seq.take 1 matches

let return = Seq.return
let (let*) = Seq.flat_map

let matches reg s =
  let end_idx = String.length s in
  let rec matches reg start =
    match reg with
    | RBeg when start = 0 -> return @@ Some (0, 0)
    | RBeg -> Seq.empty
    | REnd when start = end_idx -> return @@ Some (end_idx, end_idx)
    | REnd -> Seq.empty
    | RStar r ->
      let* x = matches r start in
      failwith "TODO"
    | ROpt r -> failwith "TODO"
    | ROr (r1, r2) -> failwith "TODO"
    | RLit f when start = end_idx -> Seq.empty
    | RLit f when f s.[start] -> return @@ Some (start, start+1)
    | RLit _ -> Seq.empty in

