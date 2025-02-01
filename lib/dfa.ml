module type Finite = sig
  include Set.OrderedType
  val values: t list
end

module type OrderedType = Set.OrderedType

module type DFA = sig
  type state
  type alphabet
  
  val initial : state
  val is_accepting : state -> bool
  val next_state : state -> alphabet -> state option
end

module type NFA = sig
  type state
  type alphabet
  
  val initial : state list
  val is_accepting : state -> bool
  val next_states : state -> alphabet -> state list
end

module Nfa2Dfa (A: Finite) (O: Set.OrderedType) (N: NFA with type alphabet = A.t and type state = O.t) = struct
  module ISet = Set.Make(Int)
  module SSet = Set.Make(O)
  module SSMap = Map.Make(SSet)

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
    let sm = ref SSMap.empty in
    let transitions = ref TMap.empty in

    let initial = fresh_state () in
    let initial_set = SSet.of_list N.initial in
    sm := SSMap.add initial_set initial !sm;

    let rec loop = function
    | [] -> ()
    | x :: xs ->
      let s = SSMap.find x !sm in
      A.values 
      |> List.fold_left (fun acc a ->
        let next =
          List.concat_map (Fun.flip N.next_states a) (SSet.elements x)
          |> SSet.of_list in
        if SSet.is_empty next then acc
        else
          match SSMap.find_opt next !sm with
          | Some next_state ->
            transitions := TMap.add (s, a) next_state !transitions;
            acc (* already visited *)
          | None -> 
            let next_state = fresh_state () in
            sm := SSMap.add next next_state !sm;
            transitions := TMap.add (s, a) next_state !transitions;
            next :: acc) xs
      |> loop
    in
    loop [initial_set];

    let accepting =
      SSMap.fold 
        (fun s st acc ->
          if SSet.exists N.is_accepting s then ISet.add st acc else acc)
        !sm ISet.empty in
    
    { accepting; transitions = !transitions; initial }

  let initial = dfa.initial
  let is_accepting s = ISet.mem s dfa.accepting
  let next_state s a = TMap.find_opt (s, a) dfa.transitions
end
