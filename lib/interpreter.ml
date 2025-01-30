open Command

type 'a operation =
| ReadLine of (string option -> 'a operation)
| WriteString of string * (unit -> 'a operation)
| Done of 'a
| ReadFile of string * (string Seq.t -> 'a operation)
| WriteFile of string * string Seq.t * (unit -> 'a operation)

module OperationMonad : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val read_line : string option t
  val write_string : string -> unit t
  val read_file : string -> string Seq.t t
  val write_file : string -> string Seq.t -> unit t
end = struct
  type 'a t = 'a operation
  let return x = Done x
  let rec (>>=) m f = match m with
    | ReadLine k -> ReadLine (fun s -> k s >>= f)
    | WriteString (s, k) -> WriteString (s, fun () -> k () >>= f)
    | Done x -> f x
    | ReadFile (file, k) -> ReadFile (file, fun s -> k s >>= f)
    | WriteFile (file, s, k) -> WriteFile (file, s, fun () -> k () >>= f)
  let bind = (>>=)
  let read_line = ReadLine (fun x -> Done x)
  let write_string s = WriteString (s, fun () -> Done ())
  let read_file f = ReadFile (f, fun x -> Done x)
  let write_file f s = WriteFile (f, s, fun () -> Done ())
end

type interpreter_state = {
  pattern_space: string;
  hold_space: string;
  line_number: int;
  command_enables: bool array; (* for address ranges *)
  input_buffer: string list;
}

let run (commands: command list): int operation =
  let state = ref {
    pattern_space="";
    hold_space="";
    line_number=0;
    command_enables=Array.make (List.length commands) false;
    input_buffer = [];
  } in
  failwith "TODO"

