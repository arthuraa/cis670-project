
open Fc_syntax 
open Util 
open Camlp4.PreCast 

module MGram = MakeGram(Lexer) 

let kind_entry = MGram.Entry.mk "kind" 
let role_entry = MGram.Entry.mk "role"
let kind_and_role_entry = MGram.Entry.mk "kind_and_role"

let watch ()= MGram.Entry.print Format.std_formatter;;
let _ = 
  let clear = MGram.Entry.clear in 
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
        x = "*/"; r = role_entry -> KR(Star,r);
      | x = TRY kind_entry; "/"; c = role_entry -> KR(x,c)
      | "("; x = SELF ;")" -> x 
      ] ] ;
  END 


let (|-) f g = fun x -> x |> f |> g 

let kind_entry_of_string  = 
  Stream.of_string |-  MGram.parse kind_entry (Loc.mk "<string>")
let role_entry_of_string  = 
  Stream.of_string |- MGram.parse role_entry (Loc.mk "<string>")
let kind_and_role_entry_of_string = 
  Stream.of_string |- MGram.parse kind_and_role_entry (Loc.mk "<string>")


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
