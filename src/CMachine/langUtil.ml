open Lang

let string_of_rte : runtimeErr -> string = function
  | InvalidArg -> "InvalidArg"
  | InvalidMemory -> "InvalidMemory"
  | InvalidPC -> "InvalidPC"
  | StackOverflow -> "StackOverflow"

let string_of_rteopt : runtimeErr option -> string = function
  | None -> "None"
  | Some rte -> string_of_rte rte

let string_of_inst : inst -> string =
  let si = string_of_int in
  function
  | HALT -> "HALT"
  (* Arithmetic *)
  | ADD -> "ADD"
  | SUB -> "SUB" 
  | MUL -> "MUL"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | NEG -> "NEG"
  (* Logical : False = 0, True = (a value except 0) *)
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  (* Comparisons : False = 0, True = (a value except 0) *)
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | LE -> "LE"
  | LEQ -> "LEQ"
  | GR -> "GR"
  | GEQ -> "GEQ"
  (* Load *)
  | LOAD m -> "LOAD " ^ si m
  | LOADA (q, m) -> "LOADA (" ^ si q ^ ", " ^ si m ^ ")"
  | LOADC q -> "LOADC " ^ si q
  | LOADR (j, m) -> "LOADR (" ^ si j ^ ", " ^ si m ^ ")"
  | LOADRC j -> "LOADRC " ^ si j
  (* Store *)
  | STORE m -> "STORE " ^ si m
  | STOREA (q, m) -> "STOREA (" ^ si q ^ ", " ^ si m ^ ")"
  | STORER (j, m) -> "STORER (" ^ si j ^ ", " ^ si m ^ ")"
  (* Jump *)
  | JUMP  n -> "JUMP " ^ si n
  | JUMPZ n -> "JUMPZ " ^ si n
  | JUMPI n -> "JUMPI " ^ si n
  (* Stack *)
  | DUP -> "DUP"
  | POP -> "POP"
  (* Memory Allocations - No FREE operation in this machine. *)
  | NEW -> "NEW"
  (* Function *)
  | ALLOC n -> "ALLOC " ^ si n
  | CALL -> "CALL"
  | ENTER n -> "ENTER " ^ si n
  | MARK -> "MARK"
  | RETURN n -> "RETURN " ^ si n
  | SLIDE (q, m) -> "SLIDE (" ^ si q ^ ", " ^ si m ^ ")"
  (* System Call *)
  | READ -> "READ"
  | WRITE -> "WRITE"
  | RANDOM -> "RANDOM"

let string_of_instarr_oneline : inst array -> string = 
  fun ia ->
  let strarr = Array.map string_of_inst ia in
  let body = Array.fold_left (fun acc x -> acc ^ x ^ "; ") "" strarr in
  "[| " ^ body ^ "|]"

let string_of_instarr_newline : inst array -> string =
  fun ia ->
  let strarr = Array.map string_of_inst ia in
  let body = Array.fold_left (fun acc x -> acc ^ x ^ ";\n" ) "" strarr in
  "[|\n" ^ body ^ "|]"
