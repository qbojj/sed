module type OrderedType = Set.OrderedType

module type Finite = sig
  include OrderedType

  val values : t list
end

module type DFA = sig
  module State : OrderedType
  module Alphabet : Finite

  val initial : State.t
  val is_accepting : State.t -> bool
  val next_state : State.t -> Alphabet.t -> State.t option
end

module type NFA = sig
  module State : OrderedType
  module Alphabet : Finite
  module StateSet : Set.S with type elt = State.t

  val initial : StateSet.t
  val is_accepting : State.t -> bool
  val next_states : State.t -> Alphabet.t -> StateSet.t
end

module type NFA_epsilon = sig
  module State : OrderedType
  module Alphabet : Finite
  module StateSet : Set.S with type elt = State.t

  type token = Epsilon | Symbol of Alphabet.t

  val initial : StateSet.t
  val is_accepting : State.t -> bool
  val next_states : State.t -> token -> StateSet.t
end

module NfaEpsilon2Nfa (N : NFA_epsilon) = struct
  module State = N.State
  module Alphabet = N.Alphabet
  module StateSet = N.StateSet

  module Impl = struct
    module SSet = StateSet
    module SMap = Map.Make (State)

    module EpsElphabet = struct
      type t = N.token = Epsilon | Symbol of Alphabet.t

      let values = Epsilon :: List.map (fun a -> Symbol a) Alphabet.values
    end

    let states =
      let states = ref N.initial in
      let rec loop () =
        let size = SSet.cardinal !states in
        let states' =
          SSet.fold
            (fun s acc ->
              EpsElphabet.values
              |> List.fold_left
                   (fun acc a -> N.next_states s a |> SSet.union acc)
                   acc)
            !states SSet.empty
        in
        states := SSet.union !states states';
        let size' = SSet.cardinal !states in
        if size = size' then () else loop ()
      in
      loop ();
      !states

    let epsilon_away =
      let epsilon_away = Hashtbl.create 51 in

      states
      |> SSet.iter (fun s ->
             let set =
               Hashtbl.find_opt epsilon_away s
               |> Option.value ~default:SSet.empty
             in
             Hashtbl.replace epsilon_away s (SSet.add s set));

      states
      |> SSet.iter (fun s ->
             let s' = N.next_states s Epsilon in
             let set =
               Hashtbl.find_opt epsilon_away s
               |> Option.value ~default:SSet.empty
             in
             Hashtbl.replace epsilon_away s (SSet.union s' set));

      let get_size () =
        Hashtbl.fold (fun _ v acc -> SSet.cardinal v + acc) epsilon_away 0
      in

      let rec loop () =
        let size = get_size () in
        (* fold x -Eps-> y -Eps-> z to x -Eps-> z *)
        states
        |> SSet.iter (fun s ->
               let set =
                 Hashtbl.find_opt epsilon_away s
                 |> Option.value ~default:SSet.empty
               in
               let set' =
                 SSet.fold
                   (fun s' acc ->
                     Hashtbl.find_opt epsilon_away s'
                     |> Option.value ~default:SSet.empty
                     |> SSet.union acc)
                   set set
               in
               Hashtbl.replace epsilon_away s set');
        let size' = get_size () in
        if size = size' then () else loop ()
      in
      loop ();
      epsilon_away

    let transitions =
      let transitions : (State.t * Alphabet.t, SSet.t) Hashtbl.t =
        Hashtbl.create 51
      in
      let add_transition s t d =
        let set =
          Hashtbl.find_opt transitions (s, t)
          |> Option.value ~default:SSet.empty
        in
        Hashtbl.replace transitions (s, t) (SSet.add d set)
      in

      Seq.product (SSet.to_seq states) (List.to_seq Alphabet.values)
      |> Seq.iter (fun (s_, c) ->
             let s = s_ in
             let s =
               Hashtbl.find_opt epsilon_away s
               |> Option.value ~default:SSet.empty
               |> SSet.add s
             in
             let s =
               SSet.fold
                 (fun s acc ->
                   N.next_states s (EpsElphabet.Symbol c) |> SSet.union acc)
                 s SSet.empty
             in
             SSet.fold
               (fun s acc ->
                 Hashtbl.find_opt epsilon_away s
                 |> Option.value ~default:SSet.empty
                 |> SSet.union acc)
               s s
             |> SSet.iter (fun d -> add_transition s_ c d));
      transitions

    let accepting =
      (* new accepting are states that have any epsilon chain to the old accepting state *)
      states
      |> SSet.filter (fun s ->
             Hashtbl.find_opt epsilon_away s
             |> Option.value ~default:SSet.empty
             |> SSet.exists N.is_accepting)

    let initial =
      N.initial |> SSet.to_seq
      |> Seq.fold_left
           (fun acc s ->
             Hashtbl.find_opt epsilon_away s
             |> Option.value ~default:SSet.empty
             |> SSet.union acc)
           SSet.empty
  end

  let initial = Impl.initial
  let is_accepting s = StateSet.mem s Impl.accepting

  let next_states s c =
    Hashtbl.find_opt Impl.transitions (s, c)
    |> Option.value ~default:StateSet.empty
end

module Nfa2Dfa (N : NFA) = struct
  module State = Int
  module Alphabet = N.Alphabet

  module Impl = struct
    module ISet = Set.Make (Int)
    module SSet = N.StateSet
    module SSMap = Map.Make (SSet)

    let alphabet_sorted =
      let v = Alphabet.values |> Array.of_list in
      Array.sort Alphabet.compare v;
      v

    let index_of_alphabet a =
      (* binsearch *)
      let rec loop l r =
        if l > r then None
        else
          let m = (l + r) / 2 in
          let v = Array.unsafe_get alphabet_sorted m in
          match Alphabet.compare a v with
          | 0 -> Some m
          | x when x < 0 -> loop l (m - 1)
          | _ -> loop (m + 1) r
      in
      loop 0 (Array.length alphabet_sorted - 1)

    let get_after x a =
      x |> SSet.to_seq
      |> Seq.map (fun s -> N.next_states s a)
      |> Seq.fold_left SSet.union SSet.empty

    let state_mapping =
      let state_mapping = ref SSMap.empty in
      let register set =
        let s = !state_mapping |> SSMap.cardinal in
        state_mapping := SSMap.add set s !state_mapping;
        ()
      in
      let rec loop = function
        | [] -> ()
        | x :: xs ->
            Alphabet.values
            |> List.fold_left
                 (fun acc a ->
                   let next = get_after x a in
                   if SSet.is_empty next then acc
                   else
                     match SSMap.find_opt next !state_mapping with
                     | Some _ -> acc
                     | None ->
                         register next;
                         next :: acc)
                 xs
            |> loop
      in

      register N.initial;
      loop [ N.initial ];

      !state_mapping

    let initial = SSMap.find N.initial state_mapping

    let transitions : State.t option array array =
      let no_states = SSMap.cardinal state_mapping in
      let transitions =
        Array.make_matrix no_states (Alphabet.values |> List.length) None
      in
      Seq.product (SSMap.to_seq state_mapping) (List.to_seq Alphabet.values)
      |> Seq.iter (fun ((s, st), a) ->
             let next = get_after s a in
             match SSMap.find_opt next state_mapping with
             | Some next_state ->
                 transitions.(st).(index_of_alphabet a |> Option.get) <-
                   Some next_state
             | None -> () (* empty set *));
      transitions

    let accepting =
      state_mapping |> SSMap.to_seq
      |> Seq.filter_map (fun (s, st) ->
             if SSet.exists N.is_accepting s then Some st else None)
      |> ISet.of_seq
  end

  let initial = Impl.initial
  let is_accepting s = Impl.ISet.mem s Impl.accepting

  let next_state s a =
    Impl.index_of_alphabet a
    |> Fun.flip Option.bind (fun i -> Impl.transitions.(s).(i))
end
