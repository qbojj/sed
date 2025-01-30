open Command


type 'a operation =
| ReadLine of (string option -> 'a operation)
| WriteString of string * (unit -> 'a operation)
| Done of 'a
| ReadFile of string * (string Seq.t -> 'a operation)
| WriteFile of string * string Seq.t * (unit -> 'a operation)

module OperationMonad = struct
  type 'a t = 'a operation
  let return x = Done x
  let rec (>>=) m f = (match m with
    | ReadLine k -> ReadLine (fun s -> k s >>= f)
    | WriteString (s, k) -> WriteString (s, fun () -> k () >>= f)
    | Done x -> f x
    | ReadFile (file, k) -> ReadFile (file, fun s -> k s >>= f)
    | WriteFile (file, s, k) -> WriteFile (file, s, fun () -> k () >>= f))
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
  let state = {
    pattern_space="";
    hold_space="";
    line_number=0;
    command_enables=Array.make (List.length commands) false;
    input_buffer = [];
  } in
  failwith "TODO"

