open Command

type compiled_program
type config = { autoprint : bool; line_wrap : int }

val compile : command list -> (compiled_program, string) Result.t
val run : config -> compiled_program -> string Seq.t -> string Seq.t
