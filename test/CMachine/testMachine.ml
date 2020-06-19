open CMachine.Machine

exception TestrunException

(* If you want shorter test log, please comment out the "printState st';" expression at the line below. *)
let dbg ist st = let st' = execute ist st in (*printState st';*) st'
let dbgsp ist st i =
  let stst = dbg ist st in
  if stst.s.(stst.sp) <> i then raise TestrunException else ()
let dbgs ist st arr = 
  let stst = dbg ist st in
  Array.iter2 (fun x y -> if x = y then () else raise TestrunException) (Array.sub stst.s 0 (stst.sp + 1)) arr
let dbgpc ist st i =
  let stst = dbg ist st in
  if stst.pc <> i then raise TestrunException else ()
let dbgnew ist st sp hpaddr =
  let stst = dbg ist st in
  if (stst.s.(stst.sp) = sp && stst.hp = hpaddr) then () else raise TestrunException
let dbgspfppc ist st sp fp pc =
  let stst = dbg ist st in
  if (stst.s.(stst.sp) = sp && stst.s.(stst.fp) = fp && stst.pc = pc)
  then () else raise TestrunException
let dbgepaddr ist st epaddr = 
  let stst = dbg ist st in
  if stst.ep = epaddr then () else raise TestrunException
let dbgso ist st = 
  let stst = dbg ist st in
  if stst.rerr = Some CMachine.Lang.StackOverflow then () else raise TestrunException
let dbgmark ist st ep fp = 
  let stst = dbg ist st in
  if stst.s.(stst.sp) = fp && stst.s.(stst.sp - 1) = ep then () else raise TestrunException
let dbgpcfpepsp ist st pc fp ep sp =
  let stst = dbg ist st in
  if stst.pc = pc && stst.fp = fp && stst.ep = ep && stst.sp = sp then () else raise TestrunException

(* empty state *)
let st0 = emptyState

(* st1: empty state for HALT *)
let st1 = copyState st0

(* st2: 3+5 for ADD *)
let st2 = 
  let st = copyState st0 in
  {st with 
    s   = [| 0;3;5; |];
    c   = [| HALT; ADD; SUB |];
    sp  = 2;
    pc  = 1;
  }

(* st3: 3-5 for SUB *)
let st3 = copyState st2

(* st4: 3*5 for MUL *)
let st4 = copyState st3 

(* st5: true for AND *)
let st5 = 
  let st = copyState st0 in
  {st with 
    s   = [| 0;1;2; |];
    c   = [| HALT; AND; SUB |];
    sp  = 2;
    pc  = 1;
  }

(* st6: false for AND *)
let st6 = 
  let st = copyState st0 in
  {st with 
    s   = [| 0;0;0; |];
    c   = [| HALT; AND; SUB |];
    sp  = 2;
    pc  = 1;
  }

(* st7: true for OR *)
let st7 =
  let st = copyState st0 in
  {st with 
    s   = [| 0;1;0; |];
    c   = [| HALT; OR; SUB |];
    sp  = 2;
    pc  = 1;
  }

(* st8: true for NEQ *)
let st8 = copyState st7

(* st9: false for LEQ *)
let st9 = copyState st8

(* st10: [|0;1;2;3;0;0;2;3|] for LOAD 2 *)
let st10 = 
  let st = copyState st0 in
  {st with 
    s   = Array.append [| 0;1;2;3;0;0;2; |] (Array.make 10 0);
    c   = [| HALT; LOAD 2; SUB |];
    sp  = 6;
    pc  = 1;
  }

(* st11: [|0;1;2;3;0;0;2;3|] for LOADA (2, 2) *)
let st11 =
  let st = copyState st0 in
  {st with 
    s   = Array.append [| 0;1;2;3;0;0; |] (Array.make 10 0);
    c   = [| HALT; LOAD 2; SUB |];
    sp  = 5;
    pc  = 1;
  }

(* st12: [|0;1;2;3;0;0;1;2;3|] for LOADR (1, 3) *)
let st12 =
  let st = copyState st0 in
  {st with 
    s   = Array.append [| 0;1;2;3;0;0; |] (Array.make 10 0);
    c   = [| HALT; LOAD 2; SUB |];
    sp  = 5;
    pc  = 1;
    fp  = 0;
  }

(* st13: [| 0;0;1;2;3;0;0;1;2;3; |] for STORE 3 *)
let st13 =
  let st = copyState st0 in
  {st with 
    s   = Array.append [| 0;0;0;0;0;0;0;1;2;3;2; |] (Array.make 10 0);
    c   = [| HALT; LOAD 2; SUB |];
    sp  = 10;
    pc  = 1;
  }

(* st14: [| 0;0;1;2;3;0;0;1;2;3; |] for STOREA (2, 3) *)
let st14 =
  let st = copyState st0 in
  {st with 
    s   = Array.append [| 0;0;0;0;0;0;0;1;2;3; |] (Array.make 10 0);
    c   = [| HALT; LOAD 2; SUB |];
    sp  = 9;
    pc  = 1;
  }

(* st15: [| 0;0;1;2;3;0;0;1;2;3; |] for STORER (1, 3) *)
let st15 =
  let st = copyState st0 in
  {st with 
    s   = Array.append [| 0;0;0;0;0;0;0;1;2;3; |] (Array.make 10 0);
    c   = [| HALT; LOAD 2; SUB |];
    sp  = 9;
    pc  = 1;
    fp  = 1;
  }

(* st16: 3 for JUMP 3 *)
let st16 =
  let st = copyState st0 in
  {st with 
    s   = (Array.make 10 0);
    c   = [| HALT; LOAD 2; SUB; HALT; |];
    sp  = 1;
    pc  = 1;
  }

(* st17: 3 for JUMPZ 3 *)
let st17 =
  let st = copyState st0 in
  {st with 
    s   = (Array.make 10 0);
    c   = [| HALT; LOAD 2; SUB; HALT; |];
    sp  = 1;
    pc  = 1;
  }

(* st18: 1 for JUMPZ 3 *)
let st18 =
  let st = copyState st0 in
  {st with 
    s   = [| 0;3;0;0 |];
    c   = [| HALT; LOAD 2; SUB; HALT; |];
    sp  = 1;
    pc  = 1;
  }

(* st19: 7 for JUMPI 4 *)
let st19 =
  let st = copyState st0 in
  {st with 
    s   = [| 0;3;0;0 |];
    c   = [| HALT; LOAD 2; SUB; HALT; |];
    sp  = 1;
    pc  = 1;
  }

(* st20: 8 for DUP *)
let st20 =
  let st = copyState st0 in
  {st with 
    s   = [| 0;8;0;0 |];
    c   = [| HALT; LOAD 2; SUB; HALT; |];
    sp  = 1;
    pc  = 1;
  }

(* st21: 7 for POP *)
let st21 = 
  let st = copyState st0 in
  {st with 
    s   = [| 0;7;7;0 |];
    c   = [| HALT; LOAD 2; SUB; HALT; |];
    sp  = 2;
    pc  = 1;
  }

(* st22: SP-> 6 <- HP for NEW *)
let st22 = 
  let st = copyState st0 in
  {st with
    s   = [| 0;0;2;0;0;0;0;0; |];
    c   = [| HALT; LOAD 2; SUB; HALT; |];
    sp  = 2;
    hp  = 8;
    pc  = 1;
  }

(* st23: SP=8 for ALLOC 6 *)
let st23 = 
  let st = copyState st0 in
  {st with
    s   = [| 0;0;2;0;0;0;0;0;(-1);0;0;0;0; |];
    c   = [| HALT; LOAD 2; SUB; HALT; |];
    sp  = 2;
    pc  = 1;
  }

(* st24: SP->3, FP->3, PC=5 for CALL *)
let st24 = 
  let st = copyState st0 in
  {st with
    s   = [| 0;0;0;0;0;0;0;5;0;0;0;0; |];
    c   = [| HALT; LOAD 2; SUB; HALT; HALT; HALT; |];
    sp  = 7;
    pc  = 3;
  }

(* st25: EP = 10 for ENTER 6 *)
let st25 = 
  let st = copyState st0 in
  {st with
    s   = [| 0;0;0;0;0;0;0;5;0;0;0;0; |];
    c   = [| HALT; LOAD 2; SUB; HALT; HALT; HALT; |];
    sp  = 4;
    pc  = 3;
  }

(* st26: raise Lang.StackOverflow for ENTER 6 *)
let st26 = 
  let st = copyState st0 in
  {st with
    s   = [| 0;0;0;0;0;0;0;0;0;0;0;0; |];
    c   = [| HALT; LOAD 2; SUB; HALT; HALT; HALT; |];
    sp  = 4;
    pc  = 3;
    hp  = 8;
  }
  
(* st27: append stack [| 10;2; |] for MARK *)
let st27 = 
  let st = copyState st0 in
  {st with
    s   = [| 0;0;0;0;5;0;0;0;0;0;0;0;0;0;0;0;0; |];
    c   = [| HALT; LOAD 2; SUB; HALT; HALT; HALT; |];
    sp  = 4;
    pc  = 3;
    ep  = 10;
    fp  = 2;
  }

(* st28: check stack result [| 0;5;6;7;8; |], and PC=2, FP=3, EP=4, SP=4 for RETURN 5 *)
let st28 = 
  let st = copyState st0 in
  {st with 
    s   = [| 0;5;6;7;8;0;0;4;3;2 |];
    c   = [| HALT; LOAD 2; SUB; HALT; HALT; HALT; HALT; LOAD 2; SUB; HALT; HALT; HALT; |];
    sp  = 9;
    pc  = 9;
    fp  = 9;
    ep  = 9;
  }

(* st29: raise Lang.StackOverflow for RETURN 5 *)
let st29 = 
  let st = copyState st0 in
  {st with 
    s   = [| 0;5;6;7;8;0;0;10;3;2;0;0;0;0; |];
    c   = [| HALT; LOAD 2; SUB; HALT; HALT; HALT; HALT; LOAD 2; SUB; HALT; HALT; HALT; |];
    sp  = 9;
    pc  = 9;
    fp  = 9;
    ep  = 9;
    hp  = 9;
  }

(* st30: slide [| 3;4;5; |] 4 units below for SLIDE (4, 3) *)
let st30 = 
  let st = copyState st0 in
  {st with
    s   = [| 0;0;0;0;0;0;0;0;3;4;5; |];
    c   = [| HALT; LOAD 2; SUB; |];
    sp  = 10;
    pc  = 1;
  }


(* run *)
let _ = print_endline "st1: HALT"
let _ = dbgsp HALT st1 0

let _ = print_endline "st2: ADD, 8"
let _ = dbgsp ADD st2 8

let _ = print_endline "st3: SUB, -2"
let _ = dbgsp SUB st3 (-2)

let _ = print_endline "st4: MUL, 15"
let _ = dbgsp MUL st4 15

let _ = print_endline "st5: AND, 1"
let _ = dbgsp AND st5 1

let _ = print_endline "st6: AND, 0"
let _ = dbgsp AND st6 0

let _ = print_endline "st7: OR, 1"
let _ = dbgsp OR st7 1

let pe s = print_endline s

let _ = pe "st8: 1 for NEQ"
let _ = dbgsp NEQ st8 1

let _ = pe "st9: 0 for LEQ"
let _ = dbgsp LEQ st9 0

let _ = pe "st10: [|0;1;2;3;0;0;2;3|] for LOAD 2"
let _ = dbgs (LOAD 2) st10 [|0;1;2;3;0;0;2;3|]

let _ = pe "st11: [|0;1;2;3;0;0;2;3|] for LOADA (2, 2)"
let _ = dbgs (LOADA (2,2)) st11 [|0;1;2;3;0;0;2;3|]

let _ = pe "st12: [|0;1;2;3;0;0;1;2;3|] for LOADR (1, 3)"
let _ = dbgs (LOADR (1, 3)) st12 [|0;1;2;3;0;0;1;2;3|]

let _ = pe "st13: [| 0;0;1;2;3;0;0;1;2;3; |] for STORE 3"
let _ = dbgs (STORE 3) st13 [| 0;0;1;2;3;0;0;1;2;3; |]

let _ = pe "st14: [| 0;0;1;2;3;0;0;1;2;3; |] for STOREA (2, 3)"
let _ = dbgs (STOREA(2,3)) st14 [| 0;0;1;2;3;0;0;1;2;3; |]

let _ = pe "st15: [| 0;0;1;2;3;0;0;1;2;3; |] for STORER (1, 3)"
let _ = dbgs (STORER(1,3)) st15 [| 0;0;1;2;3;0;0;1;2;3; |]

let _ = pe "st16: 3 for JUMP 3"
let _ = dbgpc (JUMP 3) st16 3

let _ = pe "st17: 3 for JUMPZ 3"
let _ = dbgpc (JUMPZ 3) st17 3

let _ = pe "st18: 1 for JUMPZ 3"
let _ = dbgpc (JUMPZ 3) st18 1

let _ = pe "st19: 7 for JUMPI 4"
let _ = dbgpc (JUMPI 4) st19 7

let _ = pe "st20: 8 for DUP"
let _ = dbgsp DUP st20 8

let _ = pe "st21: 7 for POP"
let _ = dbgsp POP st21 7

let _ = pe "st22: SP-> 6 = HP for NEW"
let _ = dbgnew NEW st22 6 6

let _ = pe "st23: SP=8 -> (-1) for ALLOC 6"
let _ = dbgsp (ALLOC 6) st23 (-1)

let _ = pe "st24: SP->3, FP->3, PC=5 for CALL"
let _ = dbgspfppc CALL st24 3 3 5

let _ = pe "st25: EP = 10 for ENTER 6"
let _ = dbgepaddr (ENTER 6) st25 10

let _ = pe "st26: raise Lang.StackOverflow for ENTER 6"
let _ = dbgso (ENTER 6) st26

let _ = pe "st27: append stack [| 10;2; |] for MARK"
let _ = dbgmark MARK st27 10 2

let _ = pe "st28: check stack result [| 0;5;6;7;8; |], and PC=2, FP=3, EP=4, SP=4 for RETURN 5"
let _ = dbgpcfpepsp (RETURN 5) st28 2 3 4 4

let _ = pe "st29: raise Lang.StackOverflow for RETURN 5"
let _ = dbgso (RETURN 5) st29

let _ = pe "st30: slide [| 3;4;5; |] 4 units below for SLIDE (4, 3)"
let _ = dbgs (SLIDE (4,3)) st30 [| 0;0;0;0;3;4;5; |]


let _ = print_newline (); print_endline "testrun.ml: test finished successfully."; print_newline ()
