module CMLang     = CMachine.Lang
module CMLangUtil = CMachine.LangUtil
module CMachine   = CMachine.Machine

module SCLang     = SubC.Lang
module SCLangUtil = SubC.LangUtil
module SCParser   = SubC.Parse

module Translate  = Translator.Translate

(* Executor will take the filename and show the execution result.
  Filename -> FileString -> (Lex,Parse) -> SubC-Lang -> (Compile) -> CMachine-Lang -> (CMachine-Run) -> Result
*)

(* command line flags *)
let print_source_code_flag   : bool ref = ref false
let print_compiled_code_flag : bool ref = ref false
let debugging_mode_flag      : bool ref = ref false


(* filename -> string *)
let read_whole_file filename : string =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

(* CMachine-Lang -> Executed CMachine State *)
let cmachineRun : CMLang.store -> CMachine.state =
  fun cmstore ->
  let es = CMachine.emptyState in
  let initialState = {es with c=cmstore;} in
  CMachine.run initialState

let printCompiledCode : CMLang.store -> CMLang.store = 
  fun cmstore ->
  let _ : unit = 
    if !print_compiled_code_flag
    then (
      print_endline "<<< COMPILED CODE >>>";
      CMLangUtil.string_of_instarr_newline cmstore |> print_endline;
      print_newline ();
    )
    else ()
  in cmstore

let printSourceCode : SCLang.pgm -> SCLang.pgm = 
  fun scpgm ->
  let _ : unit = 
    if !print_source_code_flag
    then (
      print_endline "<<< SOURCE CODE >>>";
      SCLangUtil.string_of_pgm scpgm |> print_endline;
      print_newline ();
    )
    else ()
  in scpgm


(* functions for debugging process *)
let cmachineInit : CMLang.store -> CMachine.state =
  fun cmstore ->
  let es = CMachine.emptyState in
  {es with c=cmstore;}

let rec dbgrun : CMachine.state -> CMachine.state = 
  fun cmstt ->
  let nextst, flag = CMachine.execute_one cmstt in
  if flag
  then (
    let pc = nextst.pc in
    let clen = Array.length nextst.c in
    print_newline ();
    print_endline "[Current Instruction]";
    print_endline ("   " ^ string_of_int (pc - 1) ^ " : " ^ if (pc > 0       ) then CMLangUtil.string_of_inst nextst.c.(pc - 1) else "[INVALID]");
    print_endline ("-> " ^ string_of_int (pc    ) ^ " : " ^ if (pc < clen    ) then CMLangUtil.string_of_inst nextst.c.(pc    ) else "[INVALID]");
    print_endline ("   " ^ string_of_int (pc + 1) ^ " : " ^ if (pc < clen - 1) then CMLangUtil.string_of_inst nextst.c.(pc + 1) else "[INVALID]");
    print_newline ();
    print_endline "[Machine State]";
    CMachine.printState nextst;
    let _ = print_newline () in
    dbgrun nextst
  )
  else nextst



(*******************************************************************)
(*******************************************************************)
(* Toplevel Run                                                    *)
(*******************************************************************)
(*******************************************************************)

let filenames : string list ref = ref []
let setFilenames s = filenames := !filenames @ [s]
let cmdParmas = [
  ("-psc", Arg.Set print_source_code_flag, "Print source code.");
  ("-pcc", Arg.Set print_compiled_code_flag, "Print compiled code.");
  ("-d",   Arg.Set debugging_mode_flag, "Debugging mode.")
]

let _ =
  let usage_msg = Printf.sprintf "Usage: %s <input-file>" Sys.argv.(0) in
  Arg.parse cmdParmas setFilenames usage_msg;
  let _ : unit = 
    if List.length !filenames = 0
    then (print_endline usage_msg; failwith "No Arguments";)
    else ()
  in
  let run (filename : string) : unit = 
    filename
    |> read_whole_file
    |> SCParser.parse
    |> printSourceCode
    |> Translate.subc_of_pgm
    |> Array.of_list
    |> printCompiledCode
    |> (fun x -> let _ : unit = print_endline "<<< EXECUTING... >>>"; in x)
    |> cmachineRun
    |> (fun x -> let _ : unit = print_endline "<<< EXECUTION RESULT >>>"; in x)
    |> CMachine.printState
  in
  let dbg (filename : string) : unit = 
  filename
    |> read_whole_file
    |> SCParser.parse
    |> printSourceCode
    |> Translate.subc_of_pgm
    |> Array.of_list
    |> printCompiledCode
    |> (fun x -> let _ : unit = print_endline "<<< EXECUTING... >>>"; in x)
    |> cmachineInit
    |> dbgrun
    |> (fun x -> let _ : unit = print_endline "<<< EXECUTION RESULT >>>"; in x)
    |> CMachine.printState
  in
  if !debugging_mode_flag 
    then dbg (List.hd !filenames)
    else run (List.hd !filenames)
