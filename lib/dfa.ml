module type Finite = sig
  include Set.OrderedType
  val values: t list
end

module type OrderedType = Set.OrderedType

module type Dfa = sig
  type state
  type alphabet
  
  val initial : state
  val is_accepting : state -> bool
  val next_state : state -> alphabet -> state option
end

module type Nfa = sig
  type state
  type alphabet
  
  val initial : state list
  val is_accepting : state -> bool
  val next_states : state -> alphabet -> state list
end

module Nfa2Dfa (A: Finite) (O: Set.OrderedType) (N: Nfa with type alphabet = A.t and type state = O.t) = struct
  module ISet = Set.Make(Int)
  module SSet = Set.Make(O)

  type state = int
  type alphabet = A.t

  module TMap = Map.Make(struct
    type t = state * alphabet
    let compare (a, a2) (b, b2) =
      match Int.compare a b with
      | 0 -> A.compare a2 b2
      | x -> x
  end)

  type t = {
    accepting: ISet.t;
    transitions: state TMap.t;
    initial: state;
  }

  let dfa =
    let fresh_state =
      let counter = ref 0 in
      fun () -> let s = !counter in counter := s + 1; s in
    let sm = Hashtbl.create 51 in
    let accepting = ref ISet.empty in
    let transitions = ref TMap.empty in
    let initial = fresh_state () in
    let initial_set = SSet.of_list N.initial in
    Hashtbl.replace sm initial_set initial;
    let rec loop = function
    | [] -> ()
    | x :: xs ->
      let s = Hashtbl.find sm x in
      if SSet.exists (Fun.flip List.mem N.initial) x then accepting := ISet.add s !accepting;
      A.values 
      |> List.to_seq
      |> Seq.filter_map (fun a ->
        let next = SSet.of_list (List.concat (List.map (fun s -> N.next_states s a) (SSet.elements x))) in
        (* if empty -> skip *)
        if SSet.is_empty next then None
        else
          match Hashtbl.find_opt sm next with
          | Some next_state ->
            transitions := TMap.add (s, a) next_state !transitions;
            None (* already visited *)
          | None -> 
            let next_state = fresh_state () in
            Hashtbl.replace sm next next_state;
            transitions := TMap.add (s, a) next_state !transitions;
            Some next)
      |> List.of_seq
      |> List.append xs
      |> loop
    in
    loop [initial_set];
    { accepting = !accepting; transitions = !transitions; initial = initial }

  let initial = dfa.initial
  let is_accepting s = ISet.mem s dfa.accepting
  let next_state s a = TMap.find_opt (s, a) dfa.transitions
end
