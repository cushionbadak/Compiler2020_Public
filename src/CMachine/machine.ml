type state = {
  (* memory *)
  s       : Lang.stack;   (* data stack *)
  c       : Lang.store;   (* program store (code) *)
  (* pointer *)
  sp      : int;          (* stack pointer *)
  pc      : int;          (* program counter *)
  hp      : int;          (* heap pointer *)
  fp      : int;          (* frame pointer *)
  ep      : int;          (* extreme pointer *)
  (* control *)
  halt    : bool;         (* halting status *)
  rerr    : Lang.runtimeErr option; (* runtime error *)
  (* metric *)
  ecount  : int;          (* count the number of execution *)
}

let stackSize = 1024

let emptyState = {
  s = Array.make stackSize 0;
  c = [| HALT |];
  sp = 0;
  pc = 0;
  hp = stackSize;
  fp = 0;
  ep = 0;
  halt = false;
  rerr = None;
  ecount = 0;
}

let customInitialMachineState : int -> Lang.store -> state =
  fun stsize code ->
  {
    s = Array.make stsize 0;
    c = code;
    sp = 0;
    pc = 0;
    hp = stsize;
    fp = 0;
    ep = 0;
    halt = false;
    rerr = None;
    ecount = 0;
  }

let copyState st = {st with s = (Array.copy st.s); c = (Array.copy st.c);}

(****************************************************************************)
(****************************************************************************)
(* Execution                                                                *)
(****************************************************************************)
(****************************************************************************)

(*
  cssr : check the stack size and run
  parameter: 
    sz : int = required stack size
    st : state = input state
    f  : state -> state = instruction details
  output :
    state
  
  Check the stack size requirement and run instead of "execute" function.
  If the state is valid to run instruction, return the argument state itself and the true value, 
  else, write error on the "rerr" entry in the state and the false value.
*)
let cssr : int -> state -> (state -> state) -> state = 
  fun sz st f ->
  if st.sp < sz
  then {st with rerr = Some Lang.InvalidMemory}
  else (f st)

let binop_execute_template : (int -> int -> int) -> (state -> state) =
  fun binop t ->
  let s = t.s in
  let sp = t.sp - 1 in
  let _ = Array.set s sp (binop s.(sp) s.(sp+1)) in
  {t with sp=sp;}

let rec execute =
  let open Lang in
  fun ist st ->
  (match ist with
    (* Program *)
    | HALT -> {st with halt=true}

    (* Arithmetic *)
    | ADD -> 
      let f = binop_execute_template ( + ) in
      cssr 2 st f

    | SUB ->
      let f = binop_execute_template ( - ) in
      cssr 2 st f

    | MUL ->
      let f = binop_execute_template ( * ) in
      cssr 2 st f

    | DIV ->
      let f = binop_execute_template ( / ) in
      cssr 2 st f

    | MOD ->
      let f = binop_execute_template ( mod ) in
      cssr 2 st f

    | NEG ->
      let f t = 
        let s = t.s in
        let sp = t.sp in
        let _ = Array.set s sp (- s.(sp)) in
        t
      in
      cssr 1 st f

    (* Logical : False = 0, True = (a value except 0) *)
    | AND ->
      let ff i1 i2 = if i1 = 0 || i2 = 0 then 0 else 1 in
      let f = binop_execute_template ff in
      cssr 2 st f

    | OR ->
      let ff i1 i2 = if i1 <> 0 || i2 <> 0 then 1 else 0 in
      let f = binop_execute_template ff in
      cssr 2 st f

    | NOT ->
      let ff i = if i = 0 then 1 else 0 in
      let f t = 
        let s = t.s in
        let sp = t.sp in
        let _ = Array.set s sp (ff s.(sp)) in
        t
      in
      cssr 1 st f

    (* Comparisons : False = 0, True = (a value except 0) *)
    | EQ ->
      let ff i1 i2 = if i1 = i2 then 1 else 0 in
      let f = binop_execute_template ff in
      cssr 2 st f

    | NEQ ->
      let ff i1 i2 = if i1 <> i2 then 1 else 0 in
      let f = binop_execute_template ff in
      cssr 2 st f

    | LE ->
      let ff i1 i2 = if i1 < i2 then 1 else 0 in
      let f = binop_execute_template ff in
      cssr 2 st f

    | LEQ ->
      let ff i1 i2 = if i1 <= i2 then 1 else 0 in
      let f = binop_execute_template ff in
      cssr 2 st f

    | GR ->
      let ff i1 i2 = if i1 > i2 then 1 else 0 in
      let f = binop_execute_template ff in
      cssr 2 st f

    | GEQ ->
      let ff i1 i2 = if i1 >= i2 then 1 else 0 in
      let f = binop_execute_template ff in
      cssr 2 st f

    (* Load *)
    | LOAD m ->
      if (m < 0)
      then {st with rerr = Some InvalidArg;}
      else (
        let f t = 
          let s = t.s in
          let sp = t.sp - 1 in
          let pv = s.(sp+1) - 1 in
          if not (pv >= 0)
          then {t with rerr = Some InvalidMemory;}
          else (
            let _ =
              for i = 1 to m do
                Array.set s (sp + i) s.(pv + i)
              done
            in
            {t with sp=(sp+m);}
          )
        in
        cssr (1+m) st f
      )

    | LOADA (q, m) ->
      if (m < 0)
      then {st with rerr = Some InvalidArg;}
      else (
        let f t =
          let t1 = execute (LOADC q) t in
          execute (LOAD m) t1
        in
        cssr m st f
      )

    | LOADC q ->
      let f t = 
        let s = t.s in
        let sp = t.sp + 1 in
        let _ = Array.set s sp q in
        {t with sp=sp;}
      in
      cssr 0 st f

    | LOADR (j, m) ->
      if (m < 0)
      then {st with rerr = Some InvalidArg;}
      else (
        let f t =
          let t1 = execute (LOADRC j) t in
          execute (LOAD m) t1
        in
        cssr m st f
      )

    | LOADRC j ->
      let f t =
        let s = t.s in
        let sp = t.sp + 1 in
        let fp = t.fp in
        let _ = Array.set s sp (fp + j) in
        {t with sp=sp;}
      in
      cssr 0 st f

    (* Store *)
    | STORE m ->
      if (m < 0)
      then {st with rerr = Some InvalidArg;}
      else (
        let f t =
          let s = t.s in
          let sp = t.sp - 1 in
          let pv = s.(sp+1) - 1 in
          if not (pv >= 0)
          then {t with rerr = Some InvalidMemory;}
          else (
            let _ =
              for i = 1 to m do
                Array.set s (pv + i) s.(sp - m + i)
              done
            in
            {t with sp=sp;}
          )
        in
        cssr (1+m) st f
      )

    | STOREA (q, m) ->
      if (m < 0)
      then {st with rerr = Some InvalidArg;}
      else (
        let f t =
          let t1 = execute (LOADC q) t in
          execute (STORE m) t1
        in
        cssr m st f
      )

    | STORER (j, m) ->
      if (m < 0)
      then {st with rerr = Some InvalidArg;}
      else (
        let f t =
          let t1 = execute (LOADRC j) t in
          execute (STORE m) t1
        in
        cssr m st f
      )

    (* Jump *)
    | JUMP n ->
      let f t = {t with pc=n;} in
      cssr 0 st f

    | JUMPZ n ->
      let f t =
        let s = t.s in
        let sp = t.sp - 1 in
        if s.(sp + 1) = 0 then {t with sp=sp; pc=n;} else {t with sp=sp;}
      in
      cssr 1 st f

    | JUMPI n ->
      let f t =
        let s = t.s in
        let sp = t.sp - 1 in
        {t with sp=sp; pc=(n + s.(sp+1));}
      in
      cssr 1 st f

    (* Stack *)
    | DUP ->
      let f t = 
        let s = t.s in
        let sp = t.sp + 1 in
        let _ = Array.set s sp (s.(sp-1)) in
        {t with sp=sp;}
      in
      cssr 0 st f
    | POP ->
      let f t = {t with sp=(t.sp - 1);} in
      cssr 1 st f

    (* Memory Allocations - No FREE operation in this machine. *)
    | NEW ->
      let f t = 
        let s = t.s in
        let sp = t.sp in
        let hp = t.hp in
        let ep = t.ep in
        if (hp - s.(sp) > ep)
        then (
          let hpnew = hp - s.(sp) in
          let _ = Array.set s sp hpnew in
          {t with hp=hpnew;}
        )
        else (
          let _ = Array.set s sp 0 in t
        )
      in
      cssr 1 st f

    (* Function *)
    | ALLOC m ->
      if (m < 0)
      then {st with rerr = Some InvalidArg;}
      else (
        let f t = 
          let sp = t.sp in
          {t with sp=(sp+m);}
        in
        cssr 0 st f
      )

    | CALL ->
      let f t =
        let s = t.s in
        let sp = t.sp in
        let pc = t.pc in
        let newpc = s.(sp) in
        let _ = Array.set s sp pc in
        {t with fp=sp; pc=newpc;}
      in
      cssr 1 st f

    | ENTER m ->
      if (m < 0)
      then {st with rerr = Some InvalidArg;}
      else (
        let f t = 
          let sp = t.sp in
          let hp = t.hp in
          let newep = sp + m in
          if (newep >= hp)
          then {t with rerr = Some StackOverflow;}
          else {t with ep=newep;}
        in
        cssr 0 st f
      )

    | MARK ->
      let f t = 
        let s = t.s in
        let sp = t.sp in
        let ep = t.ep in
        let fp = t.fp in
        let _ = Array.set s (sp + 1) ep in
        let _ = Array.set s (sp + 2) fp in
        {t with sp=(sp+2);}
      in
      cssr 0 st f

    | RETURN q ->
      if (q < 0)
      then {st with rerr = Some InvalidArg;}
      else (
        let f t = 
          let s = t.s in
          let fp = t.fp in
          let hp = t.hp in
          let newpc = s.(fp) in
          let newep = s.(fp-2) in
          if (newep >= hp)
          then {t with rerr = Some StackOverflow;}
          else (
            let newsp = fp - q in
            let newfp = s.(fp - 1) in
            {t with pc=newpc; ep=newep; sp=newsp; fp=newfp;}
          )
        in
        cssr q st f
      )

    | SLIDE (q, m) ->
      if (q < 0) || (m < 0)
      then {st with rerr = Some InvalidArg;}
      else (
        let f t = 
          let s = t.s in
          let sp = t.sp in
          if (m = 0)
          then {t with sp=(sp-q);}
          else (
            let newsp = sp - q - m in
            let _ = 
              for i = 1 to m do
                Array.set s (newsp + i) (s.(newsp + q + i))
              done
            in
            {t with sp=(newsp + m);}
          )
        in
        cssr (q+m) st f
      )

    (* System Call *)
    | READ ->
      let f t = 
        let s = t.s in
        let sp = t.sp + 1 in
        let _ = Array.set s sp (Stdlib.read_int ()) in
        {t with sp=sp;}
      in
      cssr 0 st f

    | WRITE ->
      let f t = 
        let s = t.s in
        let sp = t.sp - 1 in
        let _ = (
          match s.(sp + 1) with
          | 1 -> Stdlib.print_char ' '
          | 2 -> Stdlib.print_newline ()
          | _ -> Stdlib.print_int (s.(sp))
        ) in
        let _ = Array.set s sp 0 in
        {t with sp=sp;}
      in
      cssr 2 st f

    | RANDOM ->
      let f t = 
        let s = t.s in
        let sp = t.sp + 1 in
        let _ = Array.set s sp (Random.self_init (); Random.int (1024)) in
        {t with sp=sp;}
      in
      cssr 0 st f

  )



(****************************************************************************)
(****************************************************************************)
(* Print                                                                    *)
(****************************************************************************)
(****************************************************************************)

let print_stack : int -> state -> unit =
  fun indent st ->
  print_string (String.make indent ' ');
  print_string "[STACK from 1]->[";
  for i = 1 to (min st.sp (Array.length st.s)) do
    print_int st.s.(i);
    print_string "; "
  done;
  print_string "]\n";
  print_string (String.make indent ' ');
  print_string "...\n";
  print_string (String.make indent ' ');
  print_string "[";
  for i = (max 0 st.hp) to ((Array.length st.s) - 1) do
    print_int st.s.(i);
    print_string "; "
  done;
  print_string ("]<-[to HEAP " ^ (string_of_int ((Array.length st.s) - 1)) ^ "]")

let printState st =
  print_endline ("memorySize = " ^ (string_of_int (Array.length st.s)));
  print_endline ("codeSize   = " ^ (string_of_int (Array.length st.c)));
  print_endline "state = {";
  print_endline "  s       = "; 
  print_stack 12 st;
  print_newline ();
  print_endline ("  sp      = " ^ string_of_int st.sp);
  print_endline ("  pc      = " ^ string_of_int st.pc);
  print_endline ("  hp      = " ^ string_of_int st.hp);
  print_endline ("  fp      = " ^ string_of_int st.fp);
  print_endline ("  ep      = " ^ string_of_int st.ep);
  print_endline ("  halt    = " ^ string_of_bool st.halt);
  print_endline ("  rerr    = " ^ LangUtil.string_of_rteopt st.rerr);
  print_endline ("  ecount  = " ^ string_of_int st.ecount);
  print_endline "}"



(****************************************************************************)
(****************************************************************************)
(* Run                                                                      *)
(****************************************************************************)
(****************************************************************************)

let execute_one : state -> state * bool = 
  fun st ->
  (* check if the machine already stops *)
  if (st.halt || Option.is_some st.rerr)
    then st, false
    else (
      (* check if the program counter is valid *)
      let ir, cond = (
        try
          st.c.(st.pc), true
        with
        | Invalid_argument _ -> Lang.HALT, false
      ) in
      if not cond
      then {st with rerr = Some Lang.InvalidPC;}, false
      else (
        (* run the code *)
        let runst = {st with pc = st.pc + 1;} in
        try 
          let nextst = execute ir runst in
          {nextst with ecount = nextst.ecount + 1;}, true
        with err -> print_endline "<<< Uncaught ERROR in CMachine >>>"; printState runst; raise err 
      )
    )

let rec run : state -> state =
  fun st ->
  let newst, flag = execute_one st in
  if flag then run newst else newst
