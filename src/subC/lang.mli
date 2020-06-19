type var = string

(* unary operator *)
type uop =
  | UOP_MIN   (* -  *)
  | UOP_BANG  (* !  *)

(* binary operator *)
type bop =
  | BOP_PLUS  (* +  *)
  | BOP_MIN   (* -  *)
  | BOP_MUL   (* *  *)
  | BOP_DIV   (* /  *)
  | BOP_MOD   (* %  *)
  | BOP_AND   (* && *)
  | BOP_OR    (* || *)
  | BOP_EQ    (* == *)
  | BOP_NEQ   (* != *)
  | BOP_LT    (* <  *)
  | BOP_LE    (* <= *)
  | BOP_GT    (* >  *)
  | BOP_GE    (* >= *)

(* expression *)
type expr = 
  | EX_INT    of int
  | EX_LV     of lvalue
  | EX_ASSIGN of {lv: lvalue; aexpr: expr;}             (* assignment.      "lv = expr" *)
  | EX_UOP    of {uop: uop; uexpr: expr;}               (* unary operator.  ex. (- a)   *)
  | EX_BOP    of {bop: bop; expr1: expr; expr2: expr;}  (* binary operator. ex. (a + b) *)
  | EX_CALL   of {fn: var; args: expr list;}            (* function call    ex. f(1, a, 3) *)  

(* left value. They can be located at the left of the assignment expression. *)
and lvalue = 
  | LV_VAR of var
  | LV_DEREF of lvalue                               (* dereference.        ex. ( *a ) *)
  | LV_REF of lvalue                                 (* reference.          ex. &x     *)
  | LV_IDX of {ilv: lvalue; idx: expr;}              (* array index access. ex. a[3+5] *)
  | LV_TAG of {tlv: lvalue; tag: var;}               (* struct access.      ex. a.text *)

(* statement *)
type stmt = 
  | ST_EMPTY  (* ; *)
  | ST_EX of expr  (* e; *)
  | ST_IF of {ifcond: expr; thenb: stmt list; elseb: stmt list;}            (* "if (cond) {thenb} else {elseb}" *)
  | ST_SW of {swcond: expr; cases: sw_case list; default: stmt list;}      (* "switch (cond) {cases; default}" *)
  | ST_WH of {whcond: expr; whbody: stmt list;}                             (* "while (cond) {body}"            *)
  | ST_FOR of {init: expr; forcond: expr; iter: expr; forbody: stmt list;}  (* "for (init; cond; iter) {body}"  *)
  | ST_CONTINUE
  | ST_BREAK
  | ST_RT of expr                                                           (* ex. return (3+4);                *)

and sw_case = {case: int; cbody: stmt list;}

(* type expression *)
type texpr =
  | TEX_INT
  | TEX_ARR of {arrtyp: texpr; arrsize: int;}           (* array type.    ex. int[10] array; *)
  | TEX_PTR of texpr                                    (* pointer type.  ex. int* array;    *)
  | TEX_STR of {strname : var; strbody : vardecl list;} (* struct type.   ex. struct human {int x; int y; int z;}  *)
  | TEX_STRVAR of var                                   (* type expressed using custom struct-type name. ex. struct human *)

(* variable declaration. ex. int a; *)
and vardecl = {vname: var; vtyp: texpr;}

(* function definition. "rettyp name (args) {vars body}" *)
type fndef = {
  name  : var;          (* function name      *)
  rettyp: texpr;        (* return type        *)
  fargs : vardecl list; (* function arguments *)
  lvars : vardecl list; (* local variables    *)
  body  : stmt list;    (* function body      *)
}

(* global statement. It can be statement or function definition. *)
type gstmt =
  | GST_ST of stmt      (* statement            *)
  | GST_FD of fndef     (* function definition  *)

(* whole program *)
type pgm = {
  vardecls  : vardecl list;
  globstmts : gstmt list;
}
