
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

}


let lid = ['a'-'z']['A'-'z' 'a'-'z' '_']* 
let uid = ['A'-'Z']['A'-'z' 'a'-'z' '_']* 
let int = ['1'-'9']['0'-'9']*

let whitespace = [' ' '\t'] + 
let newline = ['\n']


rule token = parse 
  | "*"  {STAR }
  | "/"  {SLASH }
  | "->" {ARROW }
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
  | whitespace  {token lexbuf}

  | newline  {Lexing.new_line lexbuf ; token lexbuf}

  | lid as x {LIDENT x}

  | uid as x {
    try 
      Hashtbl.find keyword_tbl x 
    with Not_found -> UIDENT x 
  }

  | _ as c  {(printf "unrecognized char %c" c ; token lexbuf )}

  | eof  {EOF }


{
  let parser_of_buf entry = fun lexbuf -> 
    try 

        entry token lexbuf 

    with exn ->
      let start_pos,end_pos = 
        Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf in 
      begin 
        Parsing.(
        printf "error at %d.%d --- %d.%d"
          start_pos.pos_lnum (start_pos.pos_cnum -start_pos.pos_bol)
          end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol);
        print_endline (Printexc.to_string exn); raise exn )
      end 

  let parser_of_entry entry = fun str -> 
    let lexbuf = Lexing.from_string str in 
    parser_of_buf entry lexbuf 
    
  let parser_of_file entry = fun file -> 
    let chan = open_in file in 
    let lexbuf = Lexing.from_channel chan in 
    finally (fun _ -> close_in chan) 
      parser_of_buf entry lexbuf 
    
  let kind_p = parser_of_entry kind 
  let kind_and_role_p = parser_of_entry kind_and_role 
  let input_f = parser_of_file input 

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


  
}









