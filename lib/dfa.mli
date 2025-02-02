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

module NfaEpsilon2Nfa : functor (N : NFA_epsilon) ->
  NFA
    with module Alphabet = N.Alphabet
     and module State = N.State
     and module StateSet = N.StateSet

module Nfa2Dfa : functor (N : NFA) -> DFA with module Alphabet = N.Alphabet
