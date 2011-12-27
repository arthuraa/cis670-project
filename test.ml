

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
   "Nil", BDataCon (["a", KR(Star,Code)], nil_type) ;
   "Unit", BTConst (Star) ;
   "U", BDataCon ([], datatype_from_str "Unit")
       ]
  @ !cxt

let test_kind_1 = TyConst (TEquality Star)

let test_type name term ty =
  match Check_term.type_check !cxt term with
    | Some ty' when ty = ty' ->
      print_string ("test " ^ name ^ ": passed\n")
    | _ ->
      print_string ("test " ^ name ^ ": failed\n")

let mono_id =
  FCLam ("x", datatype_from_str "Unit", FCVar "x")

let mono_id_type =
  datatype_from_str "Unit" @> datatype_from_str "Unit"

let poly_id =
  FCTLam ("a", KR (Star, Type),
          FCLam ("x", TyVar "a",
                 FCVar "x"))

let poly_id_type =
  TyForall ("a", KR (Star, Type),
            mk_arrow_ty (TyVar "a") (TyVar "a"))

let is_empty =
  FCLam ("x", datatype_from_str "List" |$ datatype_from_str "Nat",
  FCCase (datatype_from_str "Bool", FCVar "x",
          [
    Branch("Nil", FCDatacon "True");
    Branch("Cons", FCLam("x",datatype_from_str "Nat",FCLam("xs",datatype_from_str "List" |$ datatype_from_str "Nat",FCDatacon "False")))
  ])
  )

let is_empty_type =
  (datatype_from_str "List" |$ datatype_from_str "Nat")
  @> (datatype_from_str "Bool")

let _ =
  test_type "mono_id" mono_id mono_id_type;
  test_type "poly_id" poly_id poly_id_type;
  test_type "is_empty" is_empty is_empty_type;


