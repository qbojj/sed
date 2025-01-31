type regex =
  | RBeg
  | REnd
  | RStar of regex
  | ROpt of regex
  | RLit of (char -> bool)
  | ROr of regex * regex
  | RList of regex list

type regex_replacement_item =
| Lit of string
| WholeMatch
| Group of int

type regex_replacement = regex_replacement_item list

(* 
unescaped & -> WholeMatch
unescaped \num -> Gruop num
any other -> Lit
*)
let parse_replacement (str: string): regex_replacement =
  let len = String.length str in
  let rec impl acc i =
    if i = len then acc
    else
      match str.[i] with
      | '&' -> impl (WholeMatch :: acc) (i + 1)
      | '\\' when i < len - 1 ->
        let c = str.[i + 1] in
        if c >= '0' && c <= '9' then impl (Group (Char.code c - Char.code '0') :: acc) (i + 2)
        else impl (Lit (String.make 1 '\\') :: Lit (String.make 1 c) :: acc) (i + 2)
      | c -> impl (Lit (String.make 1 c) :: acc) (i + 1) in
  impl [] 0
  (* coalesce literals *)
  |> List.fold_left (fun acc x ->
    match x, acc with
    | Lit s, Lit s' :: rest -> Lit (s ^ s') :: rest
    | _ -> x :: acc
  ) []
  
module GMap = Map.Make(Int)

type compiled_regex = regex
type match_info = {
  whole: int * int;
  groups: (int * int) GMap.t;
}

let compile (r: regex): compiled_regex = r
let rec compile_insensitive = function
  | RBeg -> RBeg
  | REnd -> REnd
  | RLit f -> RLit (fun c -> (f @@ Char.lowercase_ascii c) || (f @@ Char.uppercase_ascii c))
  | ROpt r -> ROpt (compile_insensitive r)
  | RStar r -> RStar (compile_insensitive r)
  | RList rl -> RList (List.map compile_insensitive rl)
  | ROr (r1, r2) -> ROr (compile_insensitive r1, compile_insensitive r2)

let replace_all (rep: regex_replacement) (s: string) (matches: match_info Seq.t): string =
  (* matches are non intersecting and in ascending order *)
  Seq.fold_left (fun (res, offset) { whole=(start, end_); groups } ->
    let prefix = String.sub s offset (start - offset) in
    let replace_item = function
      | Lit s -> s
      | WholeMatch -> String.sub s start (end_ - start)
      | Group i -> 
        begin match GMap.find_opt i groups with
        | None -> ""
        | Some (start, end_) -> String.sub s start (end_ - start)
        end in
    let rep = rep |> List.map replace_item |> String.concat "" in
    (res ^ prefix ^ rep, end_)
  ) ("", 0) matches
  |> fun (res, offset) -> res ^ String.sub s offset (String.length s - offset)

let replace rep s matches =
  replace_all rep s @@ Seq.take 1 matches

let return = Seq.return
let (let*) m f = Seq.flat_map f m

let flip = Seq.cons true @@ return false

let matches_from (reg: regex) (from: int) (s: string): int Seq.t =
  let end_idx = String.length s in
  let rec match_single (reg: regex) (start: int): int option Seq.t =
    match reg with
    | RBeg when start = 0 -> return @@ None
    | RBeg -> Seq.empty
    | REnd when start = end_idx -> return @@ None
    | REnd -> Seq.empty
    | RStar r ->
      let rec impl pos =
        let* fl = flip in
        if fl then return @@ if pos = start then None else Some pos
        else
          let* v = match_single r pos in
          match v with
          | None -> Seq.empty
          | Some p -> impl p in
      impl start
    | ROpt r ->
      let* fl = flip in
      if fl then return None
      else 
        let* v = match_single r start in
        begin match v with
        | None -> Seq.empty
        | Some p -> return @@ Some p
        end
    | ROr (r1, r2) -> 
      let* fl = flip in
      if fl then match_single r1 start else match_single r2 start
    | RLit _ when start = end_idx -> Seq.empty
    | RLit f when f s.[start] -> return @@ Some (start+1)
    | RLit _ -> Seq.empty
    | RList xs ->
      let rec impl pos = function
        | [] -> return @@ if pos = start then None else Some pos
        | x :: xs ->
          let* v = match_single x pos in
          impl (Option.value ~default:pos v) xs in
      impl start xs in
  match_single reg from
  |> Seq.map (Option.value ~default:from)

let matches (reg: compiled_regex) (s: string): match_info Seq.t =
  let rec get_all fst_idx () =
    if fst_idx > String.length s then
      Seq.Nil
    else
      match matches_from reg fst_idx s () with
      | Seq.Nil -> get_all (fst_idx+1) ()
      | Seq.Cons (v1, tl) ->
        let ma = Seq.fold_left max v1 tl in
        Seq.Cons ((fst_idx, ma), if ma = fst_idx then get_all (ma+1) else get_all ma) in
  (* filter empty matches that are adjustent to any non-empty matches *)
  let rec filter_empty t prev_end () =
    match t () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons ((start, end_), tl) ->
      if start = end_
      then
        if prev_end = start then filter_empty tl end_ ()
        else Seq.Cons ((start, end_), filter_empty tl end_)
      else
        Seq.Cons ((start, end_), filter_empty tl end_) in
  filter_empty (get_all 0) (-1)
  |> Seq.map (fun (start, end_) -> { whole = (start, end_); groups = GMap.empty })
