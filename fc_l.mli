


val token : Lexing.lexbuf -> Fc_p.token

val parser_of_entry :
  ((Lexing.lexbuf -> Fc_p.token) -> Lexing.lexbuf -> 'a) -> string -> 'a
val kind_p : string -> Fc_syntax.kind
