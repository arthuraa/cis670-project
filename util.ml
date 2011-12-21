
open Syntax 

let (|>) x f = f x 

let rec type_subst (var : string) (ty1 : fc_type) (ty2 : fc_type) =
  (* ty2 should be closed to avoid variable capture *)
  (match ty1 with 
    | TyVar var' ->
        if var = var' then ty2 else ty1
    | TyConst _ -> ty1
    | TyApp (ty11, ty12) -> TyApp (type_subst var ty11 ty2,
                                   type_subst var ty12 ty2)
    | TyForall (var', kr, ty1') ->
        if var = var'
        then ty1
        else TyForall (var', kr, type_subst var ty1' ty2)
  )

let rec type_subst_cxt ty_cxt types ty = 
  List.fold_left2 (fun ty (s,_) ty' -> 
                   type_subst s ty ty')
    ty ty_cxt types 

let sub_role r1 r2 =
  match r1, r2 with
    | (Code, Type) -> true
    | _ -> r1 = r2
let role_min : role -> role -> role = min
let check_cxt cxt = true 


let string_of_type_constant = function 
  |Datatype s | TFunction s -> s 
  | _ -> failwith "string_of_type_constant"

let arrow_kind = 
  KArrow(KR (Star,Type),(KArrow (KR (Star,Type),Star)))

let eq_kind k = 
  KArrow (KR (k,Code) , (KArrow (KR (k,Code) , Star))) 


let (|$) ty1 ty2 = 
  TyApp (ty1,ty2)

let eq_ty_of_kind k = 
  TyConst (TEquality k)
let mk_forall_ty (a,k) t = 
  TyForall (a,k,t) 

let mk_arrow_ty t1 t2 = 
  TyApp ((TyApp ((TyConst TArrow), t1)),t2)

let rec datacon_type (ty_cxt : type_context) 
    (ty: fc_type) = 
  List.fold_right  mk_forall_ty  ty_cxt  ty

let rec nth_kind_arg (kind :kind) n  =
  if n < 0 then invalid_arg "nth_kind_arg"
  else 
    let rec aux (k,n) =  match n,k with 
      | 0 , KArrow (f, r) -> f 
      | n, KArrow(f,r) -> aux (r, n - 1)
      | _ -> failwith " nth_kind_arg"
    in aux (kind,n)

    



let rec list_of_type (ty:fc_type) : fc_type * fc_type list =  match ty 
  with 
    | TyApp(a,b) -> let (h,t) = list_of_type a in (h, t @ [b])
    | _ -> ty,[]

  
