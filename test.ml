

open Fc_syntax
open Util 

let datatype_from_str str = TyConst (Datatype str)

let cxt = ref [] 
let (@>) = mk_arrow_ty 

let cons_type = 
    (TyVar "a") @>
    (datatype_from_str "List" |$ TyVar "a") @> 
    (datatype_from_str "List" |$ TyVar "a")


let nil_type = 
  datatype_from_str "List" |$ TyVar "a" 

let z_type = datatype_from_str "Nat"

let s_type = datatype_from_str "Nat" @> datatype_from_str "Nat"


let _ = 
  cxt := 
  ["Bool", BTConst Star ; (* H : eta *)
   "True", BDataCon ([], TyConst (Datatype "Bool")) ;(* K : delta . sigma *)
   "False", BDataCon ([], TyConst (Datatype "Bool")) ;(* K : delta . sigma *)

   "Nat", BTConst (Star) ; 
   "Z", BDataCon ([], z_type) ; 
   "S", BDataCon ([], s_type) ; 

   "List", BTConst (KArrow(KR(Star,Code),Star)) ; 
   "Cons", BDataCon (["a", KR(Star,Code)],
                     cons_type ) ; 
   "Nil", BDataCon (["a", KR(Star,Code)], nil_type) 
       ] 
  @ !cxt 


let is_empty = 
  FCLam ("x", datatype_from_str "List" |$ datatype_from_str "Nat", 
  FCCase (datatype_from_str "Bool", FCVar "x", 
          [
    Branch("Nil", FCDatacon "True");
    Branch("Cons", FCLam("x",datatype_from_str "Nat",FCLam("xs",datatype_from_str "List" |$ datatype_from_str "Nat",FCDatacon "False")))
  ])
  )


