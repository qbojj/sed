open Dfa

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

type transition =
  | Char of char
  | Epsilon

module ISet = Set.Make(Int)

type nfa_eps = {
  initial: int;
  accepting: int;
  transitions: (int * transition, ISet.t) Hashtbl.t;
}

type nfa = {
  initial: int;
  accepting: int;
  transitions: (int * char, ISet.t) Hashtbl.t;
}

type dfa = {
  initial: int;
  accepting: int -> bool;
  transitions: int -> char -> int option;
}

module CharFinite = struct
  type t = char
  let compare = Char.compare
  let values = List.init 256 Char.chr
end

let nfa_of_regex (reg: regex): nfa_eps =
  let get_fresh =
    let counter = ref 0 in
    fun () -> let s = !counter in counter := s + 1; s in
  let transitions = Hashtbl.create 51 in
  let add_transition s t s' =
    let set = Hashtbl.find_opt transitions (s, t) |> Option.value ~default:ISet.empty in
    Hashtbl.replace transitions (s, t) (ISet.add s' set) in
  let rec impl: regex -> int * int = function
    | RBeg -> failwith "Don't know"
    | REnd -> failwith "Don't know"
    | RStar r ->
      let (start, end_) = impl r in
      let s = get_fresh () in
      add_transition s Epsilon start;
      add_transition end_ Epsilon s;
      (s, s)
    | ROpt r ->
      let (start, end_) = impl r in
      let s = get_fresh () in
      let e = get_fresh () in
      add_transition s Epsilon start;
      add_transition end_ Epsilon e;
      add_transition s Epsilon e;
      (s, e)
    | RLit f ->
      let start = get_fresh () in
      let end_ = get_fresh () in
      CharFinite.values
      |> List.filter f
      |> List.iter (fun c -> add_transition start (Char c) end_);
      (start, end_)
    | ROr (r1, r2) ->
      let (start1, end1) = impl r1 in
      let (start2, end2) = impl r2 in
      let start = get_fresh () in
      let end_ = get_fresh () in
      add_transition start Epsilon start1;
      add_transition start Epsilon start2;
      add_transition end1 Epsilon end_;
      add_transition end2 Epsilon end_;
      (start, end_)
    | RList rl ->
      let (start, end_) = List.fold_left (fun (start, end_) r ->
        let (start', end') = impl r in
        add_transition end_ Epsilon start';
        (start, end')
      ) (get_fresh (), get_fresh ()) rl in
      (start, end_)
    | RGroup (r, _) -> impl r in
  let (initial, end_) = impl reg in
  { initial; accepting = end_; transitions }

let nfa_eps_to_nfa ({ initial; accepting; transitions }: nfa_eps): nfa =
  let states =
    Hashtbl.fold (fun (s, _) tgt acc -> ISet.add s @@ ISet.union tgt acc) transitions ISet.empty
    |> ISet.add initial
    |> ISet.add accepting
    |> ISet.to_list in

  let epsilon_away = Hashtbl.create 51 in
  Hashtbl.to_seq transitions
  |> Seq.filter (fun ((_, t), _) -> t = Epsilon)
  |> Seq.iter (fun ((s, _), s') ->
    let set = Hashtbl.find_opt epsilon_away s |> Option.value ~default:ISet.empty in
    Hashtbl.replace epsilon_away s (ISet.union s' set)
  );

  let get_size () = Hashtbl.fold (fun _ v acc -> ISet.cardinal v + acc) transitions 0 in

  let rec loop () =
    let size = get_size () in
    (* fold x -Eps-> y -Eps-> z to x -Eps-> z *)
    states
    |> List.iter (fun s ->
      let set = Hashtbl.find_opt epsilon_away s |> Option.value ~default:ISet.empty in
      let set' = 
        set |> ISet.fold (fun s' acc -> 
            ISet.union acc (Hashtbl.find_opt epsilon_away s' |> Option.value ~default:ISet.empty
          )) set in
      Hashtbl.replace epsilon_away s set'
    );
    let size' = get_size () in
    if size = size' then ()
    else loop () in
  loop ();
  
  let transitions' = Hashtbl.create 51 in
  let add_transition s t d =
    let set = Hashtbl.find_opt transitions' (s, t) |> Option.value ~default:ISet.empty in
    Hashtbl.replace transitions' (s, t) (ISet.add d set) in
  
  transitions
  |> Hashtbl.to_seq
  |> Seq.iter (fun ((s, t), d) ->
    match t with
    | Epsilon -> ()
    | Char c ->
      ISet.to_seq d
      |> Seq.iter (fun d' -> 
        Hashtbl.find_opt epsilon_away d' 
        |> Option.value ~default:ISet.empty
        |> ISet.iter (fun d'' -> add_transition s c d'')));
  { initial; accepting; transitions = transitions' }

let nfa_to_dfa (nfa: nfa): dfa =
  let module RegexDFA = Nfa2Dfa (CharFinite) (Int) (struct
    type state = int
    type alphabet = char

    let initial = [nfa.initial]
    let is_accepting s = s = nfa.accepting
    let next_states s c =
      Hashtbl.find_opt nfa.transitions (s, c) 
      |> Option.value ~default:ISet.empty
      |> ISet.to_list
  end) in
  { initial = RegexDFA.initial; accepting = RegexDFA.is_accepting; transitions = RegexDFA.next_state }

type compiled_regex =
  | Simple of regex
  | DFA of dfa

type match_info = {
  whole: int * int;
  groups: (int * int) GMap.t;
}

type regex_config = {
  case_insensitive: bool;
  supports_groups: bool;
  detection_only: bool;
}

let rec has_groups = function
  | RBeg | REnd | RStar _ | ROpt _ | RLit _ -> false
  | ROr (r1, r2) -> has_groups r1 || has_groups r2
  | RList rl -> List.exists has_groups rl
  | RGroup _ -> true

let rec dfa_compatible = function
  | RBeg | REnd -> false
  | RStar _ | ROpt _ | RLit _ -> true
  | ROr (r1, r2) -> dfa_compatible r1 && dfa_compatible r2
  | RList rl -> List.for_all dfa_compatible rl
  | RGroup _ -> false

let rec make_insensitive = function
  | RBeg -> RBeg
  | REnd -> REnd
  | RStar r -> RStar (make_insensitive r)
  | ROpt r -> ROpt (make_insensitive r)
  | RLit f -> RLit (fun c -> f @@ Char.lowercase_ascii c || f @@ Char.uppercase_ascii c)
  | ROr (r1, r2) -> ROr (make_insensitive r1, make_insensitive r2)
  | RList rl -> RList (List.map make_insensitive rl)
  | RGroup (r, i) -> RGroup (make_insensitive r, i)

let rec remove_groups = function
  | RBeg -> RBeg
  | REnd -> REnd
  | RStar r -> RStar (remove_groups r)
  | ROpt r -> ROpt (remove_groups r)
  | RLit f -> RLit f
  | ROr (r1, r2) -> ROr (remove_groups r1, remove_groups r2)
  | RList rl -> RList (List.map remove_groups rl)
  | RGroup (r, _) -> remove_groups r

let compile ({case_insensitive; supports_groups; detection_only}: regex_config) (reg: regex): compiled_regex =
  let supports_groups = supports_groups && has_groups reg in
  let reg = if case_insensitive then make_insensitive reg else reg in
  let reg = if not supports_groups then remove_groups reg else reg in
  
  if not supports_groups && dfa_compatible reg && detection_only then
    let nfa = nfa_of_regex reg in
    let nfa = nfa_eps_to_nfa nfa in
    let dfa = nfa_to_dfa nfa in
    DFA dfa |> ignore; (* TODO *)
    Simple reg
  else
    Simple reg

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
      Seq.Cons (m, filter_empty tl end_)

let matches_simple (reg: regex) (s: string): match_info Seq.t =
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
  filter_empty (get_all 0) (-1)

let matches_dfa (dfa: dfa) (s: string): match_info Seq.t =
  (* detection only (the matches are not exact) *)
  let rec impl pos state =
    if dfa.accepting state then
      Some { whole=0, pos; groups = GMap.empty }
    else
      if pos >= String.length s then
        None
      else
        impl (pos+1) (dfa.transitions state s.[pos] |> Option.value ~default:dfa.initial) in
  fun () -> (impl 0 dfa.initial |> Option.to_seq) ()

let matches (reg: compiled_regex) (s: string): match_info Seq.t =
  match reg with
  | Simple reg -> matches_simple reg s
  | DFA dfa -> matches_dfa dfa s
