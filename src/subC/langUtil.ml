open Lang

let string_of_uop : uop -> string = function
  | UOP_MIN  -> "-"
  | UOP_BANG -> "+"

let string_of_bop : bop -> string = function
  | BOP_PLUS -> "+"
  | BOP_MIN  -> "-"
  | BOP_MUL  -> "*"
  | BOP_DIV  -> "/"
  | BOP_MOD  -> "%"
  | BOP_AND  -> "&&"
  | BOP_OR   -> "||"
  | BOP_EQ   -> "=="
  | BOP_NEQ  -> "!="
  | BOP_LT   -> "<"
  | BOP_LE   -> "<="
  | BOP_GT   -> ">"
  | BOP_GE   -> ">="

let rec string_of_expr : expr -> string = function
  | EX_INT n    -> string_of_int n
  | EX_LV lv    -> string_of_lvalue lv
  | EX_ASSIGN c -> (string_of_lvalue c.lv) ^ " = " ^ (string_of_expr c.aexpr)
  | EX_UOP c    -> "(" ^ (string_of_uop c.uop) ^ (string_of_expr c.uexpr) ^ ")"
  | EX_BOP c    -> (string_of_expr c.expr1) ^ (string_of_bop c.bop) ^ (string_of_expr c.expr2)
  | EX_CALL c   -> c.fn ^ "(" ^ (string_of_args c.args) ^ ")"
and string_of_args : expr list -> string = function
  | [] -> ""
  | h :: [] -> string_of_expr h
  | h :: t  -> (string_of_expr h) ^ ", " ^ (string_of_args t)
and string_of_lvalue : lvalue -> string = function
  | LV_VAR v    -> v
  | LV_DEREF lv -> "(*" ^ (string_of_lvalue lv) ^ ")"
  | LV_REF lv   -> "(&" ^ (string_of_lvalue lv) ^ ")"
  | LV_IDX c    -> (string_of_lvalue c.ilv) ^ "[" ^ (string_of_expr c.idx) ^ "]"
  | LV_TAG c    -> (string_of_lvalue c.tlv) ^ "." ^ c.tag

let rec string_of_stmts : stmt list -> string = function
  | []      -> ""
  | h :: [] -> (string_of_stmt h)
  | h :: t  -> (string_of_stmt h) ^ (string_of_stmts t)

and string_of_stmt : stmt -> string = function
  | ST_EMPTY    -> ";\n"
  | ST_EX e     -> (string_of_expr e) ^ ";\n"
  | ST_IF c     -> "if (" ^ (string_of_expr c.ifcond) ^ ") {\n" ^ (string_of_stmts c.thenb) ^ "}\nelse {\n" ^ (string_of_stmts c.elseb) ^ "}\n"
  | ST_SW c     -> "switch (" ^ (string_of_expr c.swcond) ^ ") {\n" ^ (string_of_swcases c.cases) ^ (string_of_swdefault c.default) ^ "}\n"
  | ST_WH c     -> "while (" ^ (string_of_expr c.whcond) ^ ") {\n" ^ (string_of_stmts c.whbody) ^ "}\n"
  | ST_FOR c    -> "for (" ^ (string_of_expr c.init) ^ "; " ^ (string_of_expr c.forcond) ^ "; " ^ (string_of_expr c.iter) ^ ") {\n" ^ (string_of_stmts c.forbody) ^ "}\n"
  | ST_CONTINUE -> "continue;\n"
  | ST_BREAK    -> "break;\n"
  | ST_RT e     -> "return " ^ (string_of_expr e) ^ ";\n"

and string_of_swcases : sw_case list -> string = function 
  | []      -> ""
  | h :: t  -> "case " ^ (string_of_int h.case) ^ ": " ^ (string_of_stmts h.cbody) ^ (string_of_swcases t)
and string_of_swdefault : stmt list -> string = function 
  | c -> "default : " ^ (string_of_stmts c)

let rec string_of_texpr : texpr -> string = function
  | TEX_INT   -> "int"
  | TEX_ARR c -> (string_of_texpr c.arrtyp) ^ "[" ^ (string_of_int c.arrsize) ^ "]"
  | TEX_PTR e -> (string_of_texpr e) ^ "*"
  | TEX_STR c -> "struct " ^ c.strname ^ " {\n" ^ (string_of_vardecls c.strbody) ^ "}"
  | TEX_STRVAR v -> "struct " ^ v

and string_of_vardecls : vardecl list -> string = function
  | []      -> ""
  | h :: [] -> (string_of_texpr h.vtyp) ^ " " ^ h.vname ^ ";\n"
  | h :: t  -> (string_of_texpr h.vtyp) ^ " " ^ h.vname ^ ";\n" ^ (string_of_vardecls t)

let string_of_args : vardecl list -> string = function
  | []      -> ""
  | h :: [] -> (string_of_texpr h.vtyp) ^ " " ^ h.vname 
  | h :: t  -> (string_of_texpr h.vtyp) ^ " " ^ h.vname ^ ", " ^ (string_of_vardecls t)

let string_of_fndef : fndef -> string = fun c ->
  (string_of_texpr c.rettyp) ^ " " ^ c.name ^ " (" ^ (string_of_args c.fargs) ^ ") {\n" ^ (string_of_vardecls c.lvars) ^ (string_of_stmts c.body) ^ "}\n"

let rec string_of_gstmts : gstmt list -> string = function
  | [] -> ""
  | (GST_ST s) :: []  -> (string_of_stmt s)
  | (GST_ST s) :: t   -> (string_of_stmt s) ^ (string_of_gstmts t)
  | (GST_FD f) :: []  -> (string_of_fndef f)
  | (GST_FD f) :: t   -> (string_of_fndef f) ^ (string_of_gstmts t)


let string_of_pgm : pgm -> string = fun c ->
  (string_of_vardecls c.vardecls) ^ "\n" ^ (string_of_gstmts c.globstmts)
