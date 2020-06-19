type state = {
  (* memory *)
  s       : Lang.stack;   (* data stack *)
  c       : Lang.store;   (* program store *)
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

val stackSize   : int

val emptyState  : state
val copyState   : state -> state
val customInitialMachineState : int -> Lang.store -> state

val execute     : Lang.inst -> state -> state

val execute_one : state -> state * bool
val run         : state -> state

val printState  : state -> unit
