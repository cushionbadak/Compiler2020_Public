{
open Parser
exception LexicalError
let reserved_word = Hashtbl.create 31
let _ = List.iter (fun (keyword, tok) -> Hashtbl.add reserved_word keyword tok)
  [
    (* ("typedef", TW_typedef); *)
    ("int", TW_int);
    ("struct", TW_struct);

    (* ("main", TW_main); *)

    ("if", TW_if);
    ("else", TW_else);
    ("switch", TW_switch);
    ("while", TW_while);
    ("for", TW_for);
    ("continue", TW_continue);
    ("break", TW_break);
    ("return", TW_return);
    ("case", TW_case);
    ("default", TW_default);
  ]
}


let numeral = '0' | ['1'-'9']['0'-'9']* | '-'['1'-'9']['0'-'9']*
let blank = [' ' '\n' '\t' '\r']+
let word = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule start = parse
  | blank     { start lexbuf }
  | "/*" [ '0'-'9' 'a'-'z' 'A'-'Z' ' ' '_' '(' ')' '+' '-' '*' ':' ';' '[' ']' '<' '>'   ]* "*/" { start lexbuf }

  | "&&" {TS_amp2}
  | "||" {TS_mid2}
  | "==" {TS_eq2}
  | "!=" {TS_bangeq}
  | "<=" {TS_le}
  | ">=" {TS_ge}


  | "(" { TS_lparen }
  | ")" { TS_rparen }
  | "[" { TS_lbrack }
  | "]" { TS_rbrack }
  | "{" { TS_lbrace }
  | "}" { TS_rbrace }

  | ";"  { TS_scolon }
  | ":"  { TS_colon }
  | ","  { TS_comma }
  | "."  { TS_period }

  | "+" { TS_plus }
  | "-" { TS_min }
  | "*" { TS_mul }
  | "/" { TS_div }
  | "%" { TS_mod }

  | "=" { TS_eq }
  | "!" { TS_bang }
  | "&" { TS_amp }
  | "<" { TS_lt }
  | ">" { TS_gt }

  | numeral   { T_NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | word      { let word = Lexing.lexeme lexbuf in
                  try
                    Hashtbl.find reserved_word word
                  with _ -> T_STR word
              }
  | eof       { EOF }
  | _         { raise LexicalError }
