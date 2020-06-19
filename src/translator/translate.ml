open CMachine.Lang
open SubC.Lang

let subc_of_pgm = function
  | {vardecls=vdl; globstmts=gsl;} -> [] (* ASSIGNMENT-REQUIREMENTS *)
