
{
  open Fc_p 
  open Util
  open Printf 
  open Lexing
  open Fc_syntax
  let keyword_tbl = Hashtbl.create 30 

  let _ = 
    List.iter 
      (fun (s,token) -> 
        Hashtbl.add keyword_tbl s token )
      [
        "C", CODE;
        "T", TYPE; 
        "FORALL", FORALL;
        "CASE", CASE;
        "DATA", DATA;
        "WHERE", WHERE;
        "NTH", NTH;
        "SYM", SYM;
        "NEWTYPE", NEWTYPE;
        "TYPE",TYPE;
        "FAMILY",FAMILY;
        "INSTANCE",INSTANCE;
      ]
  let first = ref true
}


let lid = ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* 
let uid = ['A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']* 
let int = ['1'-'9']['0'-'9']*

let whitespace = [' ' '\t'] + 
let newline = ['\n']


rule token = parse 
  | "*"  {STAR }
  | "/"  {SLASH }
  | "->" {ARROW }
  | "=>" {FARROW}
  | "("  {LPAREN }
  | ")"  {RPAREN }
  | ":"  {COLON}
  | "::" {DOUBLECOLON}
  | "<"  {LTRI}
  | ">"  {RTRI}
  | "="  {EQ}
  | int as x {INT (int_of_string x)}
  | "@"  {AT}
  | "$" {TAPP}
  | ";"  {SEMI}
  | "{"  {LBRACKET}
  | "}"  {RBRACKET}
  | "."  {DOT}
  | "~"  {TILDE}
  | "(*" {comment lexbuf}
  | ","  {COMMA}
  | "\\" {LAMBDA}
  | whitespace  {token lexbuf}

  | newline  {Lexing.new_line lexbuf ; token lexbuf}

  | lid as x {LIDENT x}

  | uid as x {
    try 
      Hashtbl.find keyword_tbl x 
    with Not_found -> UIDENT x 
  }

  | _ as c  {(let err_msg = sprintf "unrecognized char %c\n" c in 
              prerr_endline err_msg ; 
              token lexbuf )}

  | eof  { if !first then EOF else raise End_of_file  }

and comment = parse 
  |"*)" { token lexbuf}
  |newline {Lexing.new_line lexbuf; comment lexbuf}
  | _  {comment lexbuf}
  | eof {raise End_of_file}

{
  let parser_of_buf entry = fun lexbuf -> 
    try 

        entry token lexbuf 

    with Parsing.Parse_error ->
      let start_pos,end_pos = 
        Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf in 
      let lexeme = Lexing.lexeme lexbuf  in 
      let cur_pos  = lexbuf.lex_curr_p in 
      begin 
        Parsing.(
          let err_msg = sprintf "error at %d.%d --- %d.%d %s \n" 
            start_pos.pos_lnum (start_pos.pos_cnum -start_pos.pos_bol)
            end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol)
            lexeme in 
          prerr_endline err_msg;
          let curr_pos_msg = sprintf "current position %d.%d\n" 
            cur_pos.pos_lnum (cur_pos.pos_cnum - cur_pos.pos_bol) in 
          prerr_endline curr_pos_msg ;
          raise Parsing.Parse_error
        )
      end 

  let parser_of_entry entry = fun str -> 
    let lexbuf = Lexing.from_string str in 
    parser_of_buf entry lexbuf 
    
  let parser_of_file entry = fun file -> 
    let chan = open_in file in 
    let lexbuf = Lexing.from_channel chan in 
    (* FIXME 
       finally (fun _ -> close_in chan)  
       Lexing will close it???
    *)
      parser_of_buf entry lexbuf 
    
  let kind_p = parser_of_entry kind 
  let kind_and_role_p = parser_of_entry kind_and_role 
  let input_f = parser_of_file input 
  let input_p = parser_of_entry input 
  let ty_def_p = parser_of_entry ty_def 
  let clause_p = parser_of_entry clause

  let test_ty () = 
    List.map ty_def_p 
      ["((a -> (LIST a)) -> (LIST a) )" ;
      " a -> LIST a -> LIST a "]

  (* Test part *)
  let _ = 
    let tests = ["*"; 
               "*/T -> * ";
               "*/T -> */C -> * "] in 
    let exp_result  = 
      [Fc_syntax.Star;

       Fc_syntax.KArrow (Fc_syntax.KR (Fc_syntax.Star, Fc_syntax.Type),
                         Fc_syntax.Star);

       Fc_syntax.KArrow (Fc_syntax.KR (Fc_syntax.Star, Fc_syntax.Type),
                         Fc_syntax.KArrow (Fc_syntax.KR (Fc_syntax.Star, Fc_syntax.Code),
                                           Fc_syntax.Star))]
    in 
    assert (List.for_all2 (fun x y -> x = y) exp_result (List.map kind_p tests))

  let test _ = 
    let dir = "test" in 
    let test_files = Sys.readdir dir in 
    Array.iter (fun file -> 
      if Filename.check_suffix file ".ml" then
        let file_name = (Filename.concat dir file ) in 
        prerr_endline ("check file: " ^ file_name);
        input_f file_name )
      test_files
  let rec tokens str = 
    let lexbuf = Lexing.from_string str  in 
    let rec aux lexbuf = 
      let a = token lexbuf in 
      if a <> EOF 
      then a :: aux lexbuf 
      else [] in 
     (aux lexbuf)

  (* let _ = test ()   *)
}









