
open Fc_syntax

let datatype_tbl : (string,bool ) Hashtbl.t = Hashtbl.create 30 
  
let function_tbl : (string, int ref ) Hashtbl.t= Hashtbl.create 30 

let cxt : context ref = ref [] 


(*
Hashtbl.print String.print Bool.print stdout Cxt.datatype_tbl;;
*)
