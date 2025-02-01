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

module Nfa2Dfa : functor (A: Finite) (O: Set.OrderedType) 
  (N: Nfa with type alphabet = A.t and type state = O.t) -> Dfa with type alphabet = N.alphabet and type state = int
