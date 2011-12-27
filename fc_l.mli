val keyword_tbl : (string, Fc_p.token) Hashtbl.t
val first : bool ref
val __ocaml_lex_tables : Lexing.lex_tables
val token : Lexing.lexbuf -> Fc_p.token
val __ocaml_lex_token_rec : Lexing.lexbuf -> int -> Fc_p.token
val comment : Lexing.lexbuf -> Fc_p.token
val __ocaml_lex_comment_rec : Lexing.lexbuf -> int -> Fc_p.token
val parser_of_buf :
  ((Lexing.lexbuf -> Fc_p.token) -> Lexing.lexbuf -> 'a) ->
  Lexing.lexbuf -> 'a
val parser_of_entry :
  ((Lexing.lexbuf -> Fc_p.token) -> Lexing.lexbuf -> 'a) -> string -> 'a
val parser_of_file :
  ((Lexing.lexbuf -> Fc_p.token) -> Lexing.lexbuf -> 'a) -> string -> 'a
val input_f : string -> unit
val input_p : string -> unit
val kind_p : string -> Fc_syntax.kind
val kind_and_role_p : string -> Fc_syntax.kind_and_role
val ty_def_p : string -> Fc_syntax.fc_type
val term_p : string -> Fc_syntax.fc_term
val proof_p : string -> Fc_syntax.co_proof
val ty_dec_p : string -> unit
val clause_p : string -> Fc_syntax.binder
val test_ty : unit -> Fc_syntax.fc_type list
val test : 'a -> (string * Fc_syntax.fc_type option) list array
val tokens : string -> Fc_p.token list
