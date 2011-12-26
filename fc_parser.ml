
open Fc_syntax 
open Util 
open Camlp4.PreCast 

module MGram = MakeGram(Lexer) 

let mk = MGram.Entry.mk 
let kind_entry = mk "kind" 
let role_entry = mk "role"
let kind_and_role_entry = mk "kind_and_role"

let dbg ()= MGram.Entry.print Format.std_formatter;;
let clear = MGram.Entry.clear 

let _ = 
  clear kind_entry; 
  clear role_entry; 
  clear kind_and_role_entry; 
  EXTEND MGram 
    GLOBAL: role_entry kind_entry kind_and_role_entry;
    role_entry : 
      [ ["C" -> Code | "T" -> Type ] ] ; 
    kind_entry : 
      [ 
        "arrow" RIGHTA
        [ "*/" ; r = role_entry;  "->" ; y = SELF -> 
        KArrow(KR(Star,r),y)
        | x= SELF; "/"; c = role_entry ;  "->" ; y = SELF -> 
        KArrow(KR(x,c),y)]
      | ["*" -> Star 
        |"("; x = SELF; ")" -> x ]
      ]; 
    kind_and_role_entry :
      [ [
        "("; x = SELF ; "->" ; k= kind_entry; ")"; 
         "/"; c = role_entry -> KR(KArrow(x,k),c) 
      |  x = "*/"; r = role_entry -> KR(Star,r);
      | "("; x = SELF ;")" -> x 
      ] ] ;
  END 


let (|-) f g = fun x -> x |> f |> g 

let parser_of_string entry = 
  let p = 
    Stream.of_string 
    |- MGram.parse entry (Loc.mk "<string>") in 
  fun str -> 
    try let v = p str in v 
    with 
        MGram.Loc.Exc_located(t,exn) -> begin
          MGram.Loc.print Format.std_formatter t ;
          print_newline ();
          raise exn 
        end 
      


let kind_entry_of_string  = parser_of_string kind_entry 
let role_entry_of_string  = parser_of_string role_entry 
let kind_and_role_entry_of_string = 
  parser_of_string kind_and_role_entry 


let _ = 
let xs = 
    ( kind_entry_of_string "*" , 
      kind_entry_of_string " * / C -> * / C -> *",
      kind_entry_of_string "*/C -> ( */C -> * ) ",
      kind_and_role_entry_of_string " * / C "
    )

let test = MGram.Entry.mk "test" 
let _ = EXTEND MGram GLOBAL: test ; 
  test : [ ["/C" -> 1 ] ] ; END ;;
let test_of_string = Stream.of_string |- MGram.parse test  (Loc.mk "<string>")
