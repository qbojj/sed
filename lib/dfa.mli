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

module Nfa2Dfa : functor (A: Finite) (O: Set.OrderedType) 
  (N: NFA with type alphabet = A.t and type state = O.t) -> DFA with type alphabet = N.alphabet and type state = int
