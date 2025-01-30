open Command

type 'a operation =
| ReadLine of (string option -> 'a operation)
| WriteString of string * (unit -> 'a operation)
| Done of 'a
| ReadFile of string * (string Seq.t -> 'a operation)
| WriteFile of string * string Seq.t * (unit -> 'a operation)

val run : command list -> int operation
