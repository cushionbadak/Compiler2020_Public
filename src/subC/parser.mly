%{
  open Lang
%}



/* Token - Symbols */

%token TS_amp2
%token TS_mid2
%token TS_eq2
%token TS_bangeq
%token TS_le
%token TS_ge

%token TS_lparen
%token TS_rparen
%token TS_lbrack
%token TS_rbrack
%token TS_lbrace
%token TS_rbrace

%token TS_scolon
%token TS_colon
%token TS_comma
%token TS_period

%token TS_plus
%token TS_min
%token TS_mul
%token TS_div
%token TS_mod

%token TS_eq
%token TS_bang
%token TS_amp
%token TS_lt
%token TS_gt



/* Token - Keywords */

/* %token TW_typedef */ /* deprecated */
%token TW_int
%token TW_struct

/* %token TW_main */ /* deprecated */

%token TW_if
%token TW_else
%token TW_switch
%token TW_while
%token TW_for
%token TW_continue
%token TW_break
%token TW_return
%token TW_case
%token TW_default



/* Token - Others */

%token          EOF
%token<int>     T_NUM
%token<string>  T_STR



/* Precedence */




%start r_pgm
%type <Lang.pgm> r_pgm



%%

/* Lang.pgm */
r_pgm:
  | r_vardecls r_gstmts EOF
      {
        { vardecls  = $1;
          globstmts = $2;
        }
      }
;

/* Lang.gstmt list */
r_gstmts:
  | r_stmt {[GST_ST($1)]}
  | r_fndef {[GST_FD($1)]}
  | r_gstmts r_stmt {$1 @ [GST_ST($2)]}
  | r_gstmts r_fndef {$1 @ [GST_FD($2)]}
;

/* Lang.fndef */
r_fndef:
  | r_texpr T_STR TS_lparen TS_rparen TS_lbrace r_vardecls r_stmts TS_rbrace
      {
        { name    = $2;
          rettyp  = $1;
          fargs   = [];
          lvars   = $6;
          body    = $7;
        }
      }
  | r_texpr T_STR TS_lparen r_args TS_rparen TS_lbrace r_vardecls r_stmts TS_rbrace
      {
        { name    = $2;
          rettyp  = $1;
          fargs   = $4;
          lvars   = $7;
          body    = $8;
        }
      }
;

/* Lang.vardecl list */
r_vardecls:
  |   {[]}
  | r_vardecls r_texpr T_STR TS_scolon {$1@[{vname=$3; vtyp=$2;}]}
;
r_args:
  | r_texpr T_STR {[{vname=$2; vtyp=$1;}]}
  | r_args TS_comma r_texpr T_STR {$1@[{vname=$4; vtyp=$3;}]}
;
r_struct_vardecls:
  | r_texpr T_STR TS_scolon {[{vname=$2; vtyp=$1;}]}
  | r_struct_vardecls r_texpr T_STR TS_scolon {$1@[{vname=$3; vtyp=$2;}]}
;

/* Lang.texpr */
r_texpr:
  | TW_int {TEX_INT}
  | r_texpr TS_lbrack T_NUM TS_rbrack {TEX_ARR{arrtyp=$1; arrsize=$3;}}
  | r_texpr TS_mul {TEX_PTR($1)}
  | TW_struct T_STR {TEX_STRVAR($2)}
  | TW_struct TS_lbrace r_struct_vardecls TS_rbrace {TEX_STR{strname=""; strbody=$3;}}
  | TW_struct T_STR TS_lbrace r_struct_vardecls TS_rbrace {TEX_STR{strname=$2; strbody=$4;}}
;


/* Lang.stmt list */
r_stmts:
  |   {[]}
  | r_stmt r_stmts {$1::$2}
;

/* Lang.stmt */
r_stmt:
  | TS_scolon                       {ST_EMPTY}
  | r_expr      TS_scolon           {ST_EX($1)}
  | TW_if       TS_lparen r_expr TS_rparen TS_lbrace r_stmts TS_rbrace
      { 
        ST_IF { 
          ifcond  = $3;
          thenb  = $6;
          elseb  = [];
        }
      }
  | TW_if       TS_lparen r_expr TS_rparen TS_lbrace r_stmts TS_rbrace TW_else TS_lbrace r_stmts TS_rbrace
      {
        ST_IF { 
          ifcond  = $3;
          thenb  = $6;
          elseb  = $10;
        }
      }
  | TW_switch   TS_lparen r_expr TS_rparen TS_lbrace r_sw_cases r_sw_default TS_rbrace
      {
        ST_SW { 
          swcond    = $3;
          cases     = $6;
          default   = $7;
        }
      }
  | TW_while    TS_lparen r_expr TS_rparen TS_lbrace r_stmts TS_rbrace
      {
        ST_WH { 
          whcond = $3;
          whbody = $6;
        }
      }
  | TW_for      TS_lparen r_expr TS_scolon r_expr TS_scolon r_expr TS_rparen TS_lbrace r_stmts TS_rbrace
      {
        ST_FOR {
          init    = $3;
          forcond = $5;
          iter    = $7;
          forbody = $10;
        }
      }
  | TW_continue TS_scolon           {ST_CONTINUE}
  | TW_break    TS_scolon           {ST_BREAK}
  | TW_return   r_expr    TS_scolon {ST_RT($2)}
;

/* Lang.sw_case list */
r_sw_cases:
  | TW_case T_NUM TS_colon r_stmts             {[{case=$2; cbody=$4;}]}
  | TW_case T_NUM TS_colon r_stmts r_sw_cases  {{case=$2; cbody=$4;} :: $5}
;

/* Lang.sw_default */
r_sw_default:
  | TW_default TS_colon r_stmts {$3}
;

/* Lang.lvalue */
r_lvalue:
  | T_STR                                   {LV_VAR($1)}
  | TS_lparen TS_mul    r_lvalue TS_rparen  {LV_DEREF($3)}
  | TS_lparen TS_amp    r_lvalue TS_rparen  {LV_REF($3)}
  | r_lvalue  TS_lbrack r_expr   TS_rbrack  {LV_IDX{ilv=$1; idx=$3;}}
  | r_lvalue  TS_period T_STR               {LV_TAG{tlv=$1; tag=$3;}}
;

/* Lang.expr */
r_expr:
  | r_lvalue TS_eq r_expr_1 {EX_ASSIGN{lv=$1; aexpr=$3}}
  | r_expr_1                {$1}
;
r_expr_1:
  | r_expr_1 TS_plus    r_expr_0  {EX_BOP{bop=BOP_PLUS; expr1=$1; expr2=$3;}}
  | r_expr_1 TS_min     r_expr_0  {EX_BOP{bop=BOP_MIN;  expr1=$1; expr2=$3;}}
  | r_expr_1 TS_mul     r_expr_0  {EX_BOP{bop=BOP_MUL;  expr1=$1; expr2=$3;}}
  | r_expr_1 TS_div     r_expr_0  {EX_BOP{bop=BOP_DIV;  expr1=$1; expr2=$3;}}
  | r_expr_1 TS_mod     r_expr_0  {EX_BOP{bop=BOP_MOD;  expr1=$1; expr2=$3;}}
  | r_expr_1 TS_amp2    r_expr_0  {EX_BOP{bop=BOP_AND;  expr1=$1; expr2=$3;}}
  | r_expr_1 TS_mid2    r_expr_0  {EX_BOP{bop=BOP_OR;   expr1=$1; expr2=$3;}}
  | r_expr_1 TS_eq2     r_expr_0  {EX_BOP{bop=BOP_EQ;   expr1=$1; expr2=$3;}}
  | r_expr_1 TS_bangeq  r_expr_0  {EX_BOP{bop=BOP_NEQ;  expr1=$1; expr2=$3;}}
  | r_expr_1 TS_lt      r_expr_0  {EX_BOP{bop=BOP_LT;   expr1=$1; expr2=$3;}}
  | r_expr_1 TS_le      r_expr_0  {EX_BOP{bop=BOP_LE;   expr1=$1; expr2=$3;}}
  | r_expr_1 TS_gt      r_expr_0  {EX_BOP{bop=BOP_GT;   expr1=$1; expr2=$3;}}
  | r_expr_1 TS_ge      r_expr_0  {EX_BOP{bop=BOP_GE;   expr1=$1; expr2=$3;}}
  | r_expr_0                      {$1}
;
r_expr_0:
  | T_NUM                         {EX_INT($1)}
  | r_lvalue                      {EX_LV($1)}
  | TS_min    r_expr_0            {EX_UOP{uop=UOP_MIN;  uexpr=$2;}}
  | TS_bang   r_expr_0            {EX_UOP{uop=UOP_BANG; uexpr=$2;}}
  | T_STR TS_lparen TS_rparen     {EX_CALL{fn=$1; args=[];}}
  | T_STR TS_lparen r_call_args TS_rparen {EX_CALL{fn=$1; args=$3;}}
  | TS_lparen r_expr   TS_rparen  {$2}
;

/* Lang.expr list */
r_call_args:
  | r_expr {[$1]}
  | r_call_args TS_comma r_expr {$1@[$3]}
