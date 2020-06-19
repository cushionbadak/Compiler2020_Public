(*
  Implementation details:
  - Scalar types (int and pointer) : OCaml built-in int type
  - Stack(Memory) and Store(Program) : OCaml built-in array type
  - Stack size is determined by Machine.stackSize
  - The first logical stack address is 1, not 0. Therefore, the initial stack pointer value is 0.
  - In the same manner, the initial heap pointer value is (Machine.stackSize + 1).
  - This machine can emit any non-zero values at Logical and Comparison instructions instead of "True" value.
      For "False" value, they'll emit 0.
  - Pop instruction does not change any value on stack. It just modifies the stack pointer value.
  - Some instructions should not have negative integers, however, the machine will not check whether the instruction comes with negative integer.
      If error occurs with this problem, 
  
  For runtime exceptions, check Lang.runtimeErr type.
*)

type inst =
  (* Program *)
  | HALT
  (* Arithmetic *)
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | NEG
  (* Logical : False = 0, True = (a value except 0) *)
  | AND
  | OR
  | NOT
  (* Comparisons : False = 0, True = (a value except 0) *)
  | EQ
  | NEQ
  | LE
  | LEQ
  | GR
  | GEQ
  (* Load *)
  | LOAD    of int
  | LOADA   of int * int (* LOADA (q, m) = (LOADC q; LOAD m) *)
  | LOADC   of int
  | LOADR   of int * int (* LOADR (j, m) = (LOADRC j; LOAD m) *)
  | LOADRC  of int
  (* Store *)
  | STORE   of int
  | STOREA  of int * int (* STOREA (q, m) = (LOADC q; STORE m) *)
  | STORER  of int * int (* STORER (j, m) = (LOADRC j; STORE m) *)
  (* Jump *)
  | JUMP    of int
  | JUMPZ   of int
  | JUMPI   of int
  (* Stack *)
  | DUP
  | POP
  (* Memory Allocations - No FREE operation in this machine. *)
  | NEW
  (* Function *)
  | ALLOC   of int
  | CALL
  | ENTER   of int
  | MARK
  | RETURN  of int
  | SLIDE   of int * int
  (* System Call *)
  | READ      (* Interpreter will push the value from the execution result of "Stdlib.read_int ()" *)
  | WRITE     (* Interpreter will pop two values from the stack (v1::v2::[BOTTOMS]) and send them to the following function. It returns 0. *)
              (*
                fun (v1 : int) (v2 : int) -> 
                match v1 with
                | 1 -> Stdlib.print_char ' '
                | 2 -> Stdlib.print_newline ()
                | _ -> Stdlib.print_int v2
              *)
  | RANDOM    (* Interpreter will push the value from the execution result of "Random.self_init (); Random.int (1024)" *)

type stack = int array
type store = inst array

(* Runtime Errors *)
type runtimeErr =
  | InvalidArg    (* When the negative-integer instruction argument is forbidden and the instruction has that argument. *)
  | InvalidMemory (* When the pointer dereference points to outside of the stack. (arises when the pointer is too big or 0 or negative) *)
  | InvalidPC     (* When the program counter has invalid array index. *)
  | StackOverflow (* p28. ".... A stack overflow is, thus, already detected when entering or when leaving a function call." *)
