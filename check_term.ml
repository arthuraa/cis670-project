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


let rec kind_check (cxt : context) (ty : fc_type) 
    (r : role ): kind_and_role option = match ty with 
  |TyVar x -> 
    (try 
      let (BTVar (KR (k,r1))) = List.assoc x cxt
      in 
      if 
        check_cxt cxt && sub_role r1 r then Some (KR (k,r))
      else None 
    with 
      |_ -> None )

  |TyConst TArrow ->
    Some (KR(arrow_kind,r))

  |TyConst (TEquality k)-> 
    Some (KR(eq_kind k,r))

  |TyConst h -> 
    let s = string_of_type_constant h in 
    (try
       let BTConst k = List.assoc s cxt in 
       if check_cxt cxt then 
         Some (KR (k,r))
       else None
     with |_ -> None 
    )

  |TyApp (a,b) -> 
    (try 
      let Some (KR ((KArrow (KR(k1,r2),k2)),r1)) 
          = kind_check cxt a r in 
      let Some (KR (k1',r1')) 
          = kind_check cxt b (role_min r1 r2)  in 
      if r = r1  && 
        k1 = k1' then 
        Some (KR(k2,r))
      else None
     with | _ -> None 
    )
  |TyForall (a,kr,ty) -> 
    (try 
       let Some (KR (Star,r1)) 
           = kind_check ((a,BTVar kr ):: cxt ) ty r in 
       if r = r1 then 
         Some (KR(Star,r))
       else None 
     with | _ -> None 
    )

  

let mk_forall_ty (a,k) t = 
  TyForall (a,k,t) 

let mk_arrow_ty t1 t2 = 
  TyApp ((TyApp ((TyConst TArrow), t1)),t2)

let rec datacon_type (ty_cxt : type_context) 
    (ty: fc_type) = 
  List.fold_right  mk_forall_ty  ty_cxt  ty


let rec type_cxt_check cxt = 
  List.for_all2 (fun ty (s, KR(k,r)) -> 
    match kind_check cxt ty r with 
        Some _ -> true 
      | _ -> false 
  )


let rec proof_check
    (cxt :context)
    (proof : co_proof)
    (r : role ):
    fc_type * fc_type * kind_and_role option =
  match proof with
    | CPAssump (c, types) -> (
      try
        let BCoer (delta, TyEq (ty1, ty2), r1) 
            = List.assoc c cxt   in 
        let Some (KR (k,_)) = kind_check cxt ty1 r1 in 
        if type_cxt_check cxt types delta 
          &&  sub_role r1 r 
        then 
          Some ((type_subst_cxt delta types ty1) , 
          (type_subst_cxt delta types ty2) , 
          (KR (k,r)))
        else 
          None 
      with 
        |_ -> None
    )
    | CRefl ty -> 
      
      



let rec type_check (cxt : context) 
    (term : fc_term) : fc_type option =  match term with
    | FCVar v -> (
        try
          let BTermVar ty'= List.assoc v cxt in 
          Some ty'
        with
            Not_found -> None
    )

    | FCLam (v, ty, term') ->
      (type_check ((v, BTermVar ty) :: cxt) term')

    | FCApp (t1, t2) -> (
      try 
        let Some (TyApp ((TyApp ((TyConst TArrow), ty1)), ty2)) = type_check cxt t1 in
        let Some ty3 = type_check cxt t2 in
        if ty1 = ty3 then
          Some ty2
        else None
      with _ -> None 
    )

    | FCTLam (a, kr, term') ->
      (try 
         let Some ty1 = 
           type_check ((a,BTVar kr)::cxt) term'
         in Some (TyForall (a,kr,ty1))
       with _ ->None
      )
       
    | FCTApp (term', ty) ->(
      let Some (TyForall (a',KR(k',r'),ty')) = 
         type_check cxt term' in 
      let Some (KR (k'',r'')) = 
        kind_check cxt ty r' in 
      Some (type_subst a' ty' ty)
     )

    | FCDatacon s -> 
      (try 
         let BDataCon (ty_cxt,ty) =
           List.assoc s cxt in 
         Some (datacon_type ty_cxt ty)
       with _ -> None 
      )

    |FCCase (ty,term',branches) -> 
      None 
    | FCPLam (s,(TyEq (ty1,ty2) as eq_ty),term') -> 
      (try
         let Some ty' =
           type_check 
             ((s, BCoer ([],eq_ty, Code))::cxt)
             term' in

         (** we use role Code 
             to do the kind check since it's 
             the most general role
         *)
         match  kind_check cxt ty1 Code,
           kind_check cxt ty2 Code with 
             |Some (KR (k1,_)), Some (KR (k2,_)) when k1 = k2 ->
               Some ((eq_ty_of_kind k1) 
                        |$ ty1 |$ ty2 |$ ty')
               
             |_ -> None 
       with _ -> None 
      )
    | FCPApp (term', proof) -> (
      try 
         let 
             Some (TyApp 
                     ((TyApp 
                         ((TyApp 
                             ((TyConst (TEquality k)),tya)),tyb))
                         ,sigma))
             = type_check cxt  term' in 
          if proof_check cxt proof (tya,tyb)  (KR (k,Code))
          then Some sigma
          else None 
      with _ -> None 
    )
    | FCoer (term',proof) -> (
      try 
        ty
    )
      
      

      (* ) *)


(* match ty_cxt with  *)
(*       | [] -> ty  *)
(*       | (x::xs) ->  *)
(*         mk_forall_ty x  *)
(*           (datacon_type xs ty) *)
