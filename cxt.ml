
open Fc_syntax

let datatype_tbl : (string,bool ) Hashtbl.t = Hashtbl.create 30 
  
let function_tbl : (string, int ref ) Hashtbl.t= Hashtbl.create 30 

let new_type_tbl : (string, bool) Hashtbl.t = Hashtbl.create 30 

let term_tbl : (string, (fc_term* fc_type option))
    Hashtbl.t = Hashtbl.create 30 

let cxt : context ref = ref [] 

let clear ()  = 
  Hashtbl.(clear datatype_tbl;
           clear function_tbl; 
           clear new_type_tbl;
           clear term_tbl
  )

(*
Hashtbl.print String.print Bool.print stdout Cxt.datatype_tbl;;
*)
