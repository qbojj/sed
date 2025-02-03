open Cmdliner
open Sed

let join_result_seq (input : ('a, string) Result.t Seq.t) :
    ('a Seq.t, string) Result.t =
  input
  |> Seq.fold_left
       (fun acc r ->
         match (acc, r) with
         | Ok acc, Ok r -> Ok (fun () -> Seq.Cons (r, acc))
         | Error e, Ok _ -> Error e
         | Ok _, Error e -> Error e
         | Error e0, Error e1 -> Error (e0 ^ "\n" ^ e1))
       (Ok Seq.empty)
  |> Result.map (fun s -> s |> List.of_seq |> List.rev |> List.to_seq)

let compile_souces source_list file_list =
  let sources =
    source_list |> List.to_seq
    |> Seq.mapi (fun i s -> Ok ("inline source " ^ string_of_int i, s))
  in
  let files =
    file_list |> List.to_seq
    |> Seq.map (fun f ->
           if Sys.file_exists f then
             let file = open_in f in
             let read = really_input_string file (in_channel_length file) in
             Ok ("file: " ^ f, read)
           else Error ("file not found: " ^ f))
  in

  Seq.append sources files
  |> Seq.map
       ((Fun.flip Result.bind) (fun (name, s) ->
            Command.parseScript s
            |> Result.map_error
                 (fun ({ row; column; msg } : Command.parse_error) ->
                   "parse error in " ^ name ^ " at " ^ string_of_int row ^ ":"
                   ^ string_of_int column ^ ": " ^ msg)))
  |> join_result_seq
  |> Result.map (Seq.fold_left ( @ ) [])

let get_file_stream io close =
  let rec impl () =
    try
      let line = input_line io in
      Seq.Cons (line, impl)
    with End_of_file ->
      if close then close_in io;
      Seq.Nil
  in
  impl

let get_stream (input_files : string list) : (string Seq.t, string) Result.t =
  input_files |> List.to_seq
  |> Seq.map (fun f ->
         if Sys.file_exists f then Ok (get_file_stream (open_in f) true)
         else Error ("file not found: " ^ f))
  |> join_result_seq |> Result.map Seq.concat

let run_err source_list file_list no_autoprint line_wrap inplace separate
    input_files =
  let ( let* ) = Result.bind in
  let* commands = compile_souces source_list file_list in
  let config : Interpreter.config =
    { autoprint = not no_autoprint; line_wrap }
  in
  let* program = Sed.Interpreter.compile commands in
  if List.is_empty input_files then (
    let input_stream = get_file_stream stdin false in
    let output_stream = Interpreter.run config program input_stream in
    Seq.iter print_endline output_stream;
    Ok ())
  else if separate then
    input_files |> List.to_seq
    |> Seq.map (fun f ->
           let* input_stream = get_stream [ f ] in
           let output_stream = Interpreter.run config program input_stream in
           match inplace with
           | None ->
               Seq.iter print_endline output_stream;
               Ok ()
           | Some suffix ->
               let output_stream = output_stream |> List.of_seq in
               let out_file = open_out (f ^ suffix) in
               List.iter
                 (fun s -> output_string out_file (s ^ "\n"))
                 output_stream;
               close_out out_file;
               Ok ())
    |> join_result_seq
    |> Result.map (fun _ -> ())
  else
    let* input_stream = get_stream input_files in
    let output_stream = Interpreter.run config program input_stream in
    Seq.iter print_endline output_stream;
    Ok ()

let run source_list file_list no_autoprint line_wrap inplace separate
    input_files =
  Printexc.record_backtrace true;
  let separate = separate || Option.is_some inplace in
  let input_files, source_list =
    match (source_list, input_files) with
    | [], [] -> ([], [])
    | [], f :: fs -> (fs, [ f ])
    | _ :: _, _ -> (input_files, source_list)
  in
  match
    run_err source_list file_list no_autoprint line_wrap inplace separate
      input_files
  with
  | Ok () -> ()
  | Error e ->
      Printf.eprintf "%s\n" e;
      exit 1

let cmd =
  let doc = "A simple sed-like command-line tool in OCaml" in
  let info = Cmd.info "sed" ~version:"1.0" ~doc in

  let source_arg =
    let doc = "Append script to list of source literals" in
    Arg.(
      value & opt_all string [] & info [ "e"; "expression" ] ~docv:"SCRIPT" ~doc)
  in

  let file_arg =
    let doc = "Append script file to list of source files" in
    Arg.(value & opt_all string [] & info [ "f"; "file" ] ~docv:"FILE" ~doc)
  in

  let no_autoprint_arg =
    let doc = "Suppress automatic printing of pattern space" in
    Arg.(value & flag & info [ "q"; "quiet"; "silent" ] ~doc)
  in

  let line_wrap_arg =
    let doc = "Specify line wrap length" in
    Arg.(
      value & opt int Int.max_int & info [ "l"; "line-length" ] ~docv:"N" ~doc)
  in

  let inplace_arg =
    let doc = "Edit files in place (optional suffix)" in
    Arg.(
      value
      & opt ?vopt:(Some (Some "")) (some string) None
      & info [ "i"; "in-place" ] ~docv:"SUFFIX" ~doc)
  in

  let separate_arg =
    let doc =
      "Consider files separately rather than as a single continuous stream"
    in
    Arg.(value & flag & info [ "s"; "separate" ] ~doc)
  in

  let files_arg =
    let doc = "Files to process" in
    Arg.(value & pos_all string [] & info [] ~docv:"FILE" ~doc)
  in

  Cmd.v info
    Term.(
      const run $ source_arg $ file_arg $ no_autoprint_arg $ line_wrap_arg
      $ inplace_arg $ separate_arg $ files_arg)

let () = exit (Cmd.eval cmd)
