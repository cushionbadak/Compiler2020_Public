let read_whole_file filename : string =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let filenames : string list ref = ref []
let setFilenames s = filenames := !filenames @ [s]
let cmdParmas = []

let _ =
  let usage_msg = Printf.sprintf "Usage: %s <input-file>" Sys.argv.(0) in
  Arg.parse cmdParmas setFilenames usage_msg;
  let run (filename : string) = 
    print_endline (SubC.LangUtil.string_of_pgm(SubC.Parse.parse (read_whole_file filename))) in
  List.iter run !filenames
