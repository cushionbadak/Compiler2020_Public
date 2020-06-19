let parse : string -> Lang.pgm = fun s ->
  let lexbuf = Lexing.from_string s in
  let program : Lang.pgm = Parser.r_pgm Lexer.start lexbuf in
  program
