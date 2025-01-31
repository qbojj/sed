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

(* 
unescaped & -> WholeMatch
unescaped \digit -> Gruop digit
any other -> Lit
*)
let parse_replacement (str: string): regex_replacement =
  Printf.printf "parsing replacement: %s\n" str;
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

type regex_config = {
  case_insensitive: bool;
  supports_groups: bool;
}

let compile ({case_insensitive; supports_groups}: regex_config) (reg: regex): compiled_regex =
  let rec impl = function
    | RBeg -> RBeg
    | REnd -> REnd
    | RStar r -> RStar (impl r)
    | ROpt r -> ROpt (impl r)
    | RLit f when not case_insensitive -> RLit f
    | RLit f -> RLit (fun c -> f @@ Char.lowercase_ascii c || f @@ Char.uppercase_ascii c)
    | ROr (r1, r2) -> ROr (impl r1, impl r2)
    | RList rl -> RList (List.map impl rl)
    | RGroup (r, _) when not supports_groups -> r
    | RGroup (r, i) -> RGroup (r, i) in
  impl reg

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

let matches_from (reg: regex) (from: int) (s: string): match_info Seq.t =
  let end_idx = String.length s in
  let rec match_single (reg: regex) (start: int) (groups: (int * int) GMap.t): (int option * (int * int) GMap.t) Seq.t =
    match reg with
    | RBeg when start = 0 -> return (None, groups)
    | RBeg -> Seq.empty
    | REnd when start = end_idx -> return (None, groups)
    | REnd -> Seq.empty
    | RStar r ->
      let rec impl pos groups =
        let* fl = flip in
        if fl then return @@ if pos = start then (None, groups) else (Some pos, groups)
        else
          let* v = match_single r pos groups in
          match v with
          | None, _ -> Seq.empty
          | Some p, groups -> impl p groups in
      impl start groups
    | ROpt r ->
      let* fl = flip in
      if fl then return @@ (None, groups)
      else 
        let* v = match_single r start groups in
        begin match v with
        | None, _ -> Seq.empty
        | Some _, _ -> return v
        end
    | ROr (r1, r2) -> 
      let* fl = flip in
      if fl then match_single r1 start groups else match_single r2 start groups
    | RLit _ when start = end_idx -> Seq.empty
    | RLit f when f s.[start] -> return (Some (start+1), groups)
    | RLit _ -> Seq.empty
    | RList xs ->
      let rec impl pos groups = function
        | [] -> return @@ if pos = start then (None, groups) else (Some pos, groups)
        | x :: xs ->
          let* (v, groups) = match_single x pos groups in
          impl (Option.value ~default:pos v) groups xs in
      impl start groups xs
    | RGroup (r, i) ->
      let* (v, groups) = match_single r start groups in
      let end_ = Option.value ~default:start v in
      return (v, GMap.add i (start, end_) groups)
    in
  match_single reg from GMap.empty
  |> Seq.map (fun (v, groups) -> { whole = (from, Option.value ~default:from v); groups })

let matches (reg: compiled_regex) (s: string): match_info Seq.t =
  let rec get_all fst_idx () =
    if fst_idx > String.length s then
      Seq.Nil
    else
      match matches_from reg fst_idx s () with
      | Seq.Nil -> get_all (fst_idx+1) ()
      | Seq.Cons (v1, tl) ->
        let ma = Seq.fold_left (fun acc v ->
          let (_, ae) = acc.whole in
          let (_, ve) = v.whole in
          if ve > ae then v else acc
          ) v1 tl in
        let ma_e = snd ma.whole in
        Seq.Cons (ma, if ma_e = fst_idx then get_all (ma_e+1) else get_all ma_e) in
  
  (* filter empty matches that are adjustent to any non-empty matches *)
  let rec filter_empty t prev_end () =
    match t () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons ({ whole=start, end_; _ } as m, tl) ->
      if start = end_
      then
        if prev_end = start then filter_empty tl end_ ()
        else Seq.Cons (m, filter_empty tl end_)
      else
        Seq.Cons (m, filter_empty tl end_) in
  filter_empty (get_all 0) (-1)
