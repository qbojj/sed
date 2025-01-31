open Command
open Regex

type c_address =
  | Line of int
  | Steps of int * int
  | Regex of compiled_regex
  | Last

type c_address_range = 
  | Always
  | Single of c_address
  | Range of { start: c_address; end_: c_address; }

type c_replace_info = {
  src: compiled_regex;
  dst: regex_replacement;
  multimatch: bool;
}

(* relative target position *)
type j_target = int

type c_command =
  | PrintLineNo
  | Append of string
  | Insert of string
  | AppendFile of string
  | ReplaceText of string
  | QuitAutoprint
  | Quit
  | Delete
  | DeleteLine
  | CopyToHold
  | AppendToHold
  | CopyFromHold
  | AppendFromHold
  | ExchangeHold
  | Next
  | AppendNext
  | Print
  | PrintFirst
  | PrintEscaped
  | Write of string
  | Replace of c_replace_info
  | Transliterate of (char * char) list
  | Branch of j_target
  | CondJmp of j_target

type c_cmd = {
  cmd: c_command;
  range: c_address_range;
  neg: bool;
}

type compiled_program = c_cmd list

(* match jumps and translate braces to jumps *)
let compile (commands: command list): (compiled_program, string) Result.t =
  let compile_address: address -> c_address = function
    | Line n -> Line n
    | Steps (n, m) -> Steps (n, m)
    | Command.Regex r -> Regex (Regex.compile r)
    | Last -> Last in
  let compile_address_opt: (address * bool) option -> (c_address_range * bool) = function
    | None -> Always, false
    | Some (a, neg) -> Single (compile_address a), neg in
  let compile_range: address_range -> (c_address_range * bool) = function
    | Always -> Always, false
    | Single (a, neg) -> Single (compile_address a), neg
    | Range (start, end_, neg) -> Range { start = compile_address start; end_ = compile_address end_; }, neg in
  let compile_replace_info ({ src; dst; flags }: replace_info): c_replace_info =
    let src =
      if List.mem CaseInsensitive flags then
        Regex.compile_insensitive src
      else
        Regex.compile src in
    let multimatch = List.mem MultiMatch flags in
    { src; dst; multimatch } in

  (* get label names -> global (flattend without labels) position (next op afer the label) *)
  let module LMap = Map.Make(String) in
  let rec get_labels acc i = function
    | [] -> acc, i
    | Label l :: tl -> get_labels (LMap.add l i acc) i tl
    | Block (_, cmds) :: tl ->
      let acc, i = get_labels acc (i + 1) cmds in
      get_labels acc i tl
    | _ :: tl -> get_labels acc (i + 1) tl in
  let labels, _ = get_labels LMap.empty 0 commands in

  let (let*) = Result.bind in
  let (let+) = Result.map in
  let return = Result.ok in

  let get_pos l = match LMap.find_opt l labels with | None -> Error ("no label named " ^ l) | Some v -> Ok v in

  let rec translate (i: int) (acc: c_cmd list): command list -> ((c_cmd list * int), string) Result.t = function
    | [] -> return (acc, i)
    | Label _ :: tl -> translate i acc tl
    | PrintLineNo range :: tl ->
      let (range, neg) = compile_address_opt range in
      translate (i + 1) ({ cmd = PrintLineNo; range; neg } :: acc) tl
    | Append (range, s) :: tl ->
      let (range, neg) = compile_address_opt range in
      translate (i + 1) ({ cmd = Append s; range; neg } :: acc) tl
    | Insert (range, s) :: tl ->
      let (range, neg) = compile_address_opt range in
      translate (i + 1) ({ cmd = Insert s; range; neg } :: acc) tl
    | QuitAutoprint range :: tl ->
      let (range, neg) = compile_address_opt range in
      translate (i + 1) ({ cmd = QuitAutoprint; range; neg } :: acc) tl
    | Quit range :: tl ->
      let (range, neg) = compile_address_opt range in
      translate (i + 1) ({ cmd = Quit; range; neg } :: acc) tl
    | AppendFile (range, s) :: tl ->
      let (range, neg) = compile_address_opt range in
      translate (i + 1) ({ cmd = AppendFile s; range; neg } :: acc) tl
    | Block (range, cmds) :: tl ->
      let (range, neg) = compile_range range in
      let* (acc_inner, i_end) = translate (i+1) [] cmds in
      let acc = { cmd = Branch (i_end - i); range; neg } :: acc in
      translate i_end (acc_inner @ acc) tl
    | Branch (range, l) :: tl ->
      let (range, neg) = compile_range range in
      let* target = get_pos l in
      translate (i + 1) ({ cmd = Branch (target - i); range; neg } :: acc) tl
    | ReplaceText (range, s) :: tl ->
      let (range, neg) = compile_range range in
      translate (i + 1) ({ cmd = ReplaceText s; range; neg } :: acc) tl
    | Delete range :: tl ->
      let (range, neg) = compile_range range in
      translate (i + 1) ({ cmd = Delete; range; neg } :: acc) tl
    | DeleteLine range :: tl ->
      let (range, neg) = compile_range range in
      translate (i + 1) ({ cmd = DeleteLine; range; neg } :: acc) tl
    | CopyToHold range :: tl ->
      let (range, neg) = compile_range range in
      translate (i + 1) ({ cmd = CopyToHold; range; neg } :: acc) tl
    | AppendToHold range :: tl ->
      let (range, neg) = compile_range range in
      translate (i + 1) ({ cmd = AppendToHold; range; neg } :: acc) tl
    | CopyFromHold range :: tl ->
      let (range, neg) = compile_range range in
      translate (i + 1) ({ cmd = CopyFromHold; range; neg } :: acc) tl
    | AppendFromHold range :: tl ->
      let (range, neg) = compile_range range in
      translate (i + 1) ({ cmd = AppendFromHold; range; neg } :: acc) tl
    | Next range :: tl ->
      let (range, neg) = compile_range range in
      translate (i + 1) ({ cmd = Next; range; neg } :: acc) tl
    | AppendNext range :: tl ->
      let (range, neg) = compile_range range in
      translate (i + 1) ({ cmd = AppendNext; range; neg } :: acc) tl
    | Print range :: tl ->
      let (range, neg) = compile_range range in
      translate (i + 1) ({ cmd = Print; range; neg } :: acc) tl
    | PrintFirst range :: tl ->
      let (range, neg) = compile_range range in
      translate (i + 1) ({ cmd = PrintFirst; range; neg } :: acc) tl
    | Replace (range, info) :: tl ->
      let (range, neg) = compile_range range in
      translate (i + 1) ({ cmd = Replace (compile_replace_info info); range; neg } :: acc) tl
    | CondJmp (range, l) :: tl ->
      let (range, neg) = compile_range range in
      let* target = get_pos l in
      translate (i + 1) ({ cmd = CondJmp (target - i); range; neg } :: acc) tl
    | Write (range, s) :: tl ->
      let (range, neg) = compile_range range in
      translate (i + 1) ({ cmd = Write s; range; neg } :: acc) tl
    | ExchangeHold range :: tl ->
      let (range, neg) = compile_range range in
      translate (i + 1) ({ cmd = ExchangeHold; range; neg } :: acc) tl
    | Transliterate (range, l) :: tl ->
      let (range, neg) = compile_range range in
      translate (i + 1) ({ cmd = Transliterate l; range; neg } :: acc) tl
    | PrintEscaped range :: tl ->
      let (range, neg) = compile_range range in
      translate (i + 1) ({ cmd = PrintEscaped; range; neg } :: acc) tl in
  let* v = translate 0 [] commands in
  return @@ List.rev @@ fst v

type e_cmd = {
  cmd: c_command;
  range: c_address_range;
  neg: bool;
  is_active: bool; (* state for dual range *)
}

(* if next is [] then zipper is empty *)
type zipper = {
  prev: e_cmd list;
  next: e_cmd list;
}

type state = {
  input: string Seq.t;
  lineno: int;
  pattern: string;
  hold: string;
  zipper: zipper;
  condjmp: bool; (* did any s/// perform successfully since last new line *) 
}

let rec zmove z n =
  if n = 0 then z
  else if n > 0 then
    match z.next with
    | [] -> z (* out of bounds -> do nothing, because already at the end *)
    | hd :: tl -> zmove { prev = hd :: z.prev; next = tl } (n - 1)
  else
    match z.prev with
    | [] -> { prev = List.rev z.next; next = [] } (* out of bounds -> make empty *)
    | hd :: tl -> zmove { prev = tl; next = hd :: z.next } (n + 1)

let rec restart z =
  { prev = []; next = List.rev_append z.prev z.next }

let update_active st cmd =
  match st.zipper.next with
  | [] -> failwith "No active"
  | _ :: tl -> { st with zipper = { st.zipper with next = cmd :: tl } }

let get_active st =
  match st.zipper.next with
  | [] -> None
  | hd :: _ -> Some hd

let is_empty st = st.zipper.next = []

let rec e_cmd_is_active st =
  let {lineno; pattern; input; _} = st in
  let cmd = get_active st |> Option.get in
  let match_address is_end = function
    | Line n -> if is_end then lineno >= n else lineno = n
    | Steps (n, m) -> lineno >= n && (lineno - n) mod m = 0
    | Regex r -> Regex.matches r pattern |> Seq.is_empty |> not
    | Last ->
      match input () with
      | Seq.Nil -> true
      | Seq.Cons _ -> false in
  match cmd.range with
  | Always -> st, true
  | Single a -> st, match_address false a <> cmd.neg
  | Range { start; end_ } ->
    if cmd.is_active then
      (if match_address true end_ then st else update_active st { cmd with is_active=false }), true <> cmd.neg
    else
      if match_address false start
      then update_active st { cmd with is_active = true }, true <> cmd.neg
      else st, false <> cmd.neg
  
type config = {
  autoprint: bool;
  line_wrap: int;
}

let split_line s =
  match String.index_opt s '\n' with
  | None -> s, None
  | Some n -> String.sub s 0 n, Some (String.sub s (n+1) (String.length s - n - 1))

let line_wrap_and_escape wrap (s: string): string =
  let rec wrap' n s () =
    match s () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (hd, tl) ->
      let escaped = String.escaped (String.make 1 hd) in
      let el = String.length escaped in
      let escaped = String.to_seq escaped in
      if n + el > wrap then
        Seq.Cons('\\', fun () -> Seq.Cons('\n', Seq.append escaped (wrap' el tl)))
      else
        (Seq.append escaped (wrap' (n + el) tl)) () in
  s
  |> String.to_seq
  |> wrap' 0
  |> String.of_seq

let run ({ autoprint; line_wrap }: config) (prog: compiled_program) (input: string Seq.t): string Seq.t =
  (* None is the new file *)
  let rec get_line st =
    match st.input () with
    | Seq.Nil -> None, { st with condjmp=false }
    | Seq.Cons (hd, tl) -> Some hd, { st with lineno=st.lineno+1; input=tl; condjmp=false }

  and new_cycle_with autoprint to_print st =
    let zipper = restart st.zipper in
    if autoprint then
      Seq.Cons (to_print, run' { st with zipper })
    else
      run' { st with zipper } ()
  
  and new_cycle autoprint st =
    let pattern = st.pattern in
    match get_line st with
    | Some line, st -> new_cycle_with autoprint pattern { st with pattern=line }
    | None, _ -> if autoprint then Seq.return pattern () else Seq.Nil
  
  and run' (st: state): string Seq.t = fun () ->
    if is_empty st then new_cycle autoprint st
    else
      let (st, is_active) = e_cmd_is_active st in
      let nst = { st with zipper = zmove st.zipper 1 } in
      if not is_active then run' nst ()
      else
        let {lineno; pattern; hold; _} = st in
        let {cmd; _} = get_active st |> Option.get in
        match cmd with
        | PrintLineNo ->
          Seq.Cons (string_of_int lineno, run' nst)
        | Append s ->
          run' {nst with pattern=pattern ^ s} ()
        | Insert s ->
          run' {nst with pattern=s} ()
        | QuitAutoprint ->
          if autoprint then Seq.return pattern () else Seq.Nil
        | Quit ->
          Seq.Nil
        | AppendFile s ->
          let file = open_in s in
          run' { nst with pattern = pattern ^ input_line file } ()
        | ReplaceText s ->
          let file = open_in s in
          run' { nst with pattern = input_line file } ()
        | Delete ->
          new_cycle false nst
        | DeleteLine ->
          (* delete first line from pattern *)
          (* if there was no new line -> start new cycle *)
          begin match split_line pattern with
          | _, None -> new_cycle false st
          | _, Some rest -> new_cycle_with false "" { nst with pattern=rest }
          end
        | CopyToHold -> run' { nst with hold = pattern } ()
        | AppendToHold -> run' { nst with hold = hold ^ pattern } ()
        | CopyFromHold -> run' { nst with pattern = hold } ()
        | AppendFromHold -> run' { nst with pattern = pattern ^ hold } ()
        | ExchangeHold -> run' { nst with pattern = hold; hold = pattern } ()
        | Next ->
          begin match get_line nst with
          | Some pattern, nst -> run' { nst with pattern } ()
          | None, nst -> run' { nst with pattern="" } ()
          end
        | AppendNext ->
          begin match get_line nst with
          | Some line, nst -> run' { nst with pattern=pattern ^ line } ()
          | None, nst -> run' nst ()
          end
        | Print -> Seq.Cons (pattern, run' nst)
        | PrintFirst ->
          let fst_line, _ = split_line pattern in
          Seq.Cons (fst_line, run' nst)
        | PrintEscaped ->
          Seq.Cons (line_wrap_and_escape line_wrap pattern, run' nst)
        | Replace info ->
          let matches = Regex.matches info.src pattern in
          let pattern = (if info.multimatch then Regex.replace_all else Regex.replace) info.dst pattern matches in
          run' { nst with pattern; condjmp=not @@ Seq.is_empty matches } ()
        | Transliterate l ->
          let pattern = String.map (fun c -> match List.assoc_opt c l with None -> c | Some c -> c) pattern in
          run' { nst with pattern } ()
        | Write s ->
          let file = open_out s in
          output_string file pattern;
          close_out file;
          run' nst ()
        | Branch n ->
          run' { nst with zipper = zmove st.zipper n } ()
        | CondJmp n ->
          if st.condjmp then run' { nst with zipper = zmove st.zipper n; condjmp=false } ()
          else run' nst () in
  let input = Seq.memoize input in
  let prog: e_cmd list = List.map (fun ({ cmd; range; neg }: c_cmd)  -> { cmd; range; neg; is_active=false }) prog in
  fun () ->
    match input () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (hd, tl) ->
      run' { input=tl; lineno=1; pattern=hd; hold=""; zipper={ prev=[]; next=prog }; condjmp=false } ()
