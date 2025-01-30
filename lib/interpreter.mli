open Command

module Make
  (FileReader : sig val read_file : string -> string end)
  (FileWriter : sig val write_file : string -> string -> unit end) : sig
  open FileReader
  open FileWriter

  val run : command list -> string Seq.t -> string Seq.t
end
