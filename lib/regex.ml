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

type regex_replacement_item = Lit of string | WholeMatch | Group of int
type regex_replacement = regex_replacement_item list

module GMap = Map.Make (Int)
module ISet = Set.Make (Int)

type transition = Epsilon | Symbol of char

type nfa_eps = {
  initial : int;
  accepting : int;
  transitions : (int * transition, ISet.t) Hashtbl.t;
}

type 'a simple_matcher_group = {
  hat_end : 'a;
  end_ : 'a;
  hat : 'a;
  mid : 'a;
}
type re_to_nfa_bstate = { he : int; e : int; h : int; mid : int }

module CharFinite = struct
  type t = char

  let compare = Char.compare
  let values = List.init 256 Char.chr
end

type match_info = { whole : int * int; groups : (int * int) GMap.t }
type compiled_regex = string -> match_info Seq.t

type regex_config = {
  case_insensitive : bool;
  supports_groups : bool;
  detection_only : bool;
}

(* 
unescaped & -> WholeMatch
unescaped \digit -> Gruop digit
any other -> Lit
*)
let parse_replacement (str : string) : regex_replacement =
  let len = String.length str in
  let rec impl acc i =
    if i = len then acc
    else
      match str.[i] with
      | '&' -> impl (WholeMatch :: acc) (i + 1)
      | '\\' when i < len - 1 ->
          let c = str.[i + 1] in
          if c >= '0' && c <= '9' then
            impl (Group (Char.code c - Char.code '0') :: acc) (i + 2)
          else
            impl
              (Lit (String.make 1 '\\') :: Lit (String.make 1 c) :: acc)
              (i + 2)
      | c -> impl (Lit (String.make 1 c) :: acc) (i + 1)
  in
  impl [] 0
  (* coalesce literals *)
  |> List.fold_left
       (fun acc x ->
         match (x, acc) with
         | Lit s, Lit s' :: rest -> Lit (s ^ s') :: rest
         | _ -> x :: acc)
       []

let nfa_eps_of_regex (reg : regex) (is_holding : bool) : nfa_eps simple_matcher_group =
  let get_fresh =
    let counter = ref 0 in
    fun () ->
      let s = !counter in
      counter := s + 1;
      s
  in
  let transitions = Hashtbl.create 51 in
  let add_transition s t s' =
    let set =
      Hashtbl.find_opt transitions (s, t) |> Option.value ~default:ISet.empty
    in
    Hashtbl.replace transitions (s, t) (ISet.add s' set)
  in
  let get_b_fresh () =
    let he = get_fresh () in
    let e = get_fresh () in
    let h = get_fresh () in
    let mid = get_fresh () in
    add_transition h Epsilon mid;
    add_transition h Epsilon he;
    add_transition he Epsilon e;
    add_transition mid Epsilon e;
    { he; e; h; mid } in
  let add_b_transition { he; e; h; mid } t s' =
    add_transition mid t s'.mid;
    if t = Epsilon then
      add_transition he Epsilon s'.he;
      add_transition h Epsilon s'.h;
      add_transition e Epsilon s'.e in
  let initial = get_b_fresh () in
  let accepting = get_b_fresh () in
  let rec impl : regex -> re_to_nfa_bstate * re_to_nfa_bstate = function
    | RBeg ->
        let start = get_b_fresh () in
        let end_ = get_b_fresh () in
        add_transition start.h Epsilon end_.h;
        add_transition start.he Epsilon end_.he;
        (start, end_)
    | REnd ->
        let start = get_b_fresh () in
        let end_ = get_b_fresh () in
        add_transition start.e Epsilon end_.e;
        add_transition start.he Epsilon end_.he;
        (start, end_)
    | RStar r ->
        let start, end_ = impl r in
        let s = get_b_fresh () in
        add_b_transition s Epsilon start;
        add_b_transition end_ Epsilon s;
        (s, s)
    | ROpt r ->
        let start, end_ = impl r in
        let s = get_b_fresh () in
        let e = get_b_fresh () in
        add_b_transition s Epsilon start;
        add_b_transition end_ Epsilon e;
        add_b_transition s Epsilon e;
        (s, e)
    | RLit f ->
        let start = get_b_fresh () in
        let end_ = get_b_fresh () in
        CharFinite.values |> List.filter f
        |> List.iter (fun c -> add_transition start.mid (Symbol c) end_.mid);
        (start, end_)
    | ROr (r1, r2) ->
        let start1, end1 = impl r1 in
        let start2, end2 = impl r2 in
        let start = get_b_fresh () in
        let end_ = get_b_fresh () in
        add_b_transition start Epsilon start1;
        add_b_transition start Epsilon start2;
        add_b_transition end1 Epsilon end_;
        add_b_transition end2 Epsilon end_;
        (start, end_)
    | RList [] ->
        let v = get_b_fresh () in
        (v, v)
    | RList (x :: rl) ->
        let start, end_ = impl x in
        let end' =
          List.fold_left
            (fun end_ r ->
              let start', end' = impl r in
              add_b_transition end_ Epsilon start';
              end')
            end_ rl
        in
        (start, end')
    | RGroup (r, _) -> impl r
  in
  let start, end_ = impl reg in
  add_b_transition initial Epsilon start;
  add_b_transition end_ Epsilon accepting;

  if is_holding then
    CharFinite.values
    |> List.iter (fun c -> 
      add_transition initial.mid (Symbol c) initial.mid;
      add_transition accepting.mid (Symbol c) accepting.mid);

  {
    hat_end={ initial=initial.h; accepting=accepting.e; transitions };
    hat={ initial=initial.h; accepting=accepting.mid; transitions };
    end_={ initial=initial.mid; accepting=accepting.e; transitions };
    mid={ initial=initial.mid; accepting=accepting.mid; transitions };
  }

module NFAEps2NFAEps (F : sig val v : nfa_eps end) = struct
  module State = Int
  module Alphabet = CharFinite
  module StateSet = ISet

  type token = transition = Epsilon | Symbol of char

  let initial = ISet.singleton F.v.initial
  let is_accepting s = s = F.v.accepting

  let next_states s t =
    Hashtbl.find_opt F.v.transitions (s, t) |> Option.value ~default:ISet.empty
end

module NFAEps2DFA (F: sig val v : nfa_eps end) = Nfa2Dfa (NfaEpsilon2Nfa (NFAEps2NFAEps (F)))

let whole_matcher_of_nfa (nfa_eps : nfa_eps simple_matcher_group) : compiled_regex =
  let module RegexDFA = NFAEps2DFA (struct let v = nfa_eps.hat_end end) in
  let match_dfa (s : string) : match_info Seq.t =
    fun () ->
      let s_end = String.fold_left
        (fun state c ->
          RegexDFA.next_state state c |> Option.value ~default:RegexDFA.initial)
        RegexDFA.initial s in
      if RegexDFA.is_accepting s_end then
        Seq.Cons ({ whole = (0, String.length s); groups = GMap.empty }, Seq.empty)
      else
        Seq.Nil
  in
  match_dfa

let any_matcher_of_nfa (nfa_eps : nfa_eps simple_matcher_group) : compiled_regex =
  let module HatEnd = NFAEps2DFA (struct let v = nfa_eps.hat_end end) in
  let module Hat = NFAEps2DFA (struct let v = nfa_eps.hat end) in
  let module End = NFAEps2DFA (struct let v = nfa_eps.end_ end) in
  let module Mid = NFAEps2DFA (struct let v = nfa_eps.mid end) in
  let match_dfa (s : string) : match_info Seq.t =
    let match_hat () =
      let rec impl state offset longest =
        let longest = if Hat.is_accepting state then Some offset else longest in
        if offset >= String.length s then longest
        else
          match Hat.next_state state s.[offset] with
          | None -> longest
          | Some state' -> impl state' (offset + 1) longest in
      let rec impl_end state offset =
        if offset >= String.length s then
          if HatEnd.is_accepting state then Some offset else None
        else
          match HatEnd.next_state state s.[offset] with
          | None -> None
          | Some state' -> impl_end state' (offset + 1) in
      match impl_end HatEnd.initial 0 with
      | None -> impl Hat.initial 0 None
      | Some v -> Some v in
    let match_from from =
      let rec impl state offset longest =
        let longest = if Mid.is_accepting state then Some offset else longest in
        if offset >= String.length s then longest
        else
          match Mid.next_state state s.[offset] with
          | None -> longest
          | Some state' -> impl state' (offset + 1) longest in
      let rec impl_end state offset =
        if offset >= String.length s then
          if End.is_accepting state then Some offset else None
        else
          match End.next_state state s.[offset] with
          | None -> None
          | Some state' -> impl_end state' (offset + 1) in
      match impl_end End.initial from with
      | None -> impl Mid.initial from None
      | Some v -> Some v in
    let rec match_rest from () =
      if from > String.length s then Seq.Nil
      else
        match match_from from with
        | None -> match_rest (from + 1) ()
        | Some end_ ->
            Seq.Cons ({ whole = (from, end_); groups = GMap.empty }, match_rest (max (from + 1) end_)) in
    fun () ->
      match match_hat () with
      | None -> match_rest 1 ()
      | Some start -> Seq.Cons ({ whole = (0, start); groups = GMap.empty }, match_rest (max 1 start)) in
  match_dfa

let rec has_groups = function
  | RBeg | REnd | RStar _ | ROpt _ | RLit _ -> false
  | ROr (r1, r2) -> has_groups r1 || has_groups r2
  | RList rl -> List.exists has_groups rl
  | RGroup _ -> true

let rec make_insensitive = function
  | RBeg -> RBeg
  | REnd -> REnd
  | RStar r -> RStar (make_insensitive r)
  | ROpt r -> ROpt (make_insensitive r)
  | RLit f ->
      RLit
        (fun c ->
          (f @@ Char.lowercase_ascii c) || (f @@ Char.uppercase_ascii c))
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

let replace_all (rep : regex_replacement) (s : string)
    (matches : match_info Seq.t) : string =
  (* matches are non intersecting and in ascending order *)
  Seq.fold_left
    (fun (res, offset) { whole = start, end_; groups } ->
      let prefix = String.sub s offset (start - offset) in
      let replace_item = function
        | Lit s -> s
        | WholeMatch -> String.sub s start (end_ - start)
        | Group i -> (
            match GMap.find_opt i groups with
            | None -> ""
            | Some (start, end_) -> String.sub s start (end_ - start))
      in
      let rep = rep |> List.map replace_item |> String.concat "" in
      (res ^ prefix ^ rep, end_))
    ("", 0) matches
  |> fun (res, offset) -> res ^ String.sub s offset (String.length s - offset)

let replace rep s matches = replace_all rep s @@ Seq.take 1 matches
let return = Seq.return
let ( let* ) m f = Seq.flat_map f m
let flip = Seq.cons true @@ return false

let matches_from (reg : regex) (from : int) (s : string) : match_info Seq.t =
  let end_idx = String.length s in
  let rec match_single (reg : regex) (start : int) (groups : (int * int) GMap.t)
      : (int option * (int * int) GMap.t) Seq.t =
    match reg with
    | RBeg when start = 0 -> return (None, groups)
    | RBeg -> Seq.empty
    | REnd when start = end_idx -> return (None, groups)
    | REnd -> Seq.empty
    | RStar r ->
        let rec impl pos groups =
          let* fl = flip in
          if fl then
            return @@ if pos = start then (None, groups) else (Some pos, groups)
          else
            let* v = match_single r pos groups in
            match v with
            | None, _ -> Seq.empty
            | Some p, groups -> impl p groups
        in
        impl start groups
    | ROpt r -> (
        let* fl = flip in
        if fl then return @@ (None, groups)
        else
          let* v = match_single r start groups in
          match v with None, _ -> Seq.empty | Some _, _ -> return v)
    | ROr (r1, r2) ->
        let* fl = flip in
        if fl then match_single r1 start groups
        else match_single r2 start groups
    | RLit _ when start = end_idx -> Seq.empty
    | RLit f when f s.[start] -> return (Some (start + 1), groups)
    | RLit _ -> Seq.empty
    | RList xs ->
        let rec impl pos groups = function
          | [] ->
              return
              @@ if pos = start then (None, groups) else (Some pos, groups)
          | x :: xs ->
              let* v, groups = match_single x pos groups in
              impl (Option.value ~default:pos v) groups xs
        in
        impl start groups xs
    | RGroup (r, i) ->
        let* v, groups = match_single r start groups in
        let end_ = Option.value ~default:start v in
        return (v, GMap.add i (start, end_) groups)
  in
  match_single reg from GMap.empty
  |> Seq.map (fun (v, groups) ->
         { whole = (from, Option.value ~default:from v); groups })

(* filter empty matches that are adjustent to any non-empty matches *)
let rec filter_empty t prev_end () =
  match t () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (({ whole = start, end_; _ } as m), tl) ->
      if start = end_ then
        if prev_end = start then filter_empty tl end_ ()
        else Seq.Cons (m, filter_empty tl end_)
      else Seq.Cons (m, filter_empty tl end_)

let matches_simple (reg : regex) (s : string) : match_info Seq.t =
  let rec get_all fst_idx () =
    if fst_idx > String.length s then Seq.Nil
    else
      match matches_from reg fst_idx s () with
      | Seq.Nil -> get_all (fst_idx + 1) ()
      | Seq.Cons (v1, tl) ->
          let ma =
            Seq.fold_left
              (fun acc v ->
                let _, ae = acc.whole in
                let _, ve = v.whole in
                if ve > ae then v else acc)
              v1 tl
          in
          let ma_e = snd ma.whole in
          Seq.Cons
            (ma, if ma_e = fst_idx then get_all (ma_e + 1) else get_all ma_e)
  in
  filter_empty (get_all 0) (-1)

let compile
    ({ case_insensitive; supports_groups; detection_only } : regex_config)
    (reg : regex) : compiled_regex =
  let supports_groups = supports_groups && has_groups reg in
  let reg = if case_insensitive then make_insensitive reg else reg in
  let reg = if not supports_groups then remove_groups reg else reg in

  if (not supports_groups) then
    if detection_only then
      nfa_eps_of_regex reg true |> whole_matcher_of_nfa
    else
      nfa_eps_of_regex reg false |> any_matcher_of_nfa
  else matches_simple reg

let matches (reg : compiled_regex) (s : string) : match_info Seq.t = reg s
