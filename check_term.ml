open Fc_syntax
open Util

let pr = prerr_endline 
 
let rec kind_check (cxt : context) (ty : fc_type)
    (r : role ): kind_and_role option = match ty with
  |TyVar x ->
    (try
      let (BTVar (KR (k,r1))) = List.assoc x cxt
      in
      if
        check_cxt cxt && sub_role r1 r then Some (KR (k,r))
      else 
        (pr "error in branch TyVar in function kind_check"; 
         None)
      
    with
      |_ -> 
        (pr "error in branch TyVar in function kind_check" ; None )
    )
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
       else 
         (pr "error in branch TyConst in function kind_check"; 
         None)
     with |_ -> 
       (pr "error in branch TyConst in function kind_check";
        None)
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
      else 
        (pr "error in branch TyApp in function kind_check";
         None
        )
     with | _ -> 
       (pr "error in branch TyApp in function kind_check "; 
       None)
    )
  |TyForall (a,kr,ty) ->
    (try
       let Some (KR (Star,r1))
           = kind_check ((a,BTVar kr ):: cxt ) ty r in
       if r = r1 then
         Some (KR(Star,r))
       else (pr "error in branch TyForall in function kind_check";
             None)
     with | _ -> 
       (pr "error in branch TyForall in function kind_check ";
       None)
    )


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
    (fc_type * fc_type * kind_and_role) option =
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
          (pr "error in branch CPAssump in function proof_check "; 
          None)
      with
        |_ -> 
          (pr "error in branch CPAssump in function proof_check";
           None)
    )
    | CPRefl ty -> (
      try
        let Some kr = kind_check cxt ty r in
        Some (ty,ty, kr)
      with _ -> 
        (pr "error in branch CPRefl in function proof_check";
         None)
    )
    |CPSym proof -> (
      try
        let Some (ty1,ty2,kr)=  proof_check cxt proof r in
        Some (ty2,ty1,kr)
      with _ -> 
        (pr "error in branch CPSym in function proof_check";
         None)
    )
    |CPTrans (a,b) -> (
      match proof_check cxt a r , proof_check cxt b r with
        | Some (ty1,ty2,kr), Some (ty2',ty3,kr') when
            ty2 = ty2' && kr = kr' ->
          Some (ty1,ty3,kr)
        | _ -> 
          (pr "error in branch CPTrans in function proof_check";
           None)
    )
    |CPApp (pf,pa) -> (
      try
        let Some (ty1,ty2, KR(KArrow (KR(k1',r2'),k2'),r'))
            = proof_check cxt pf r in
        let Some (ty1',ty2', KR(k1'', r'')) =
          proof_check cxt pa (role_min r' r2') in
        if k1' = k1''
        && r = r' (* verbose check *)
        then
          Some ((ty1 |$ ty1'), (ty2 |$ ty2'), KR(k2',r'))
        else 
          (pr "error in branch CPApp in function proof_check ";
          None)
      with _ -> 
        (pr "error in branch CPApp in function proof_check";
         None)
    )
    |CPNth (n,pf) -> (
      try
        let Some (ty1,ty2,KR(k',r'))
            = proof_check cxt pf r in
        (match list_of_type ty1, list_of_type ty2 with
          |(ht1,lst1), (ht2,lst2) ->
            if ht1 = ht2
            then
              (match ht1 with
                  (* injective in normal datatype *)
                  TyConst (Datatype s) ->
                    let BTConst k =List.assoc s cxt in
                    let (KR (k',r1)) = nth_kind_arg k n in
                    if sub_role r1 r
                    then
                      Some
                        (List.nth lst1 n,
                         List.nth lst2 n,KR(k',r))
                    else 
                      (pr "error in branch CPNth in function proof_check";
                       None)
                |_  -> 
                  (pr "error in branch CPNth in function proof_check";
                   None)
              )
            else 
              (pr "error in branch CPNth in function proof_check";
              None))
      with _ -> 
        (pr "error in branch CPNth in function proof_check";
         None)
    )

    |CPForall (a, (KR(k',r') as kr) ,pf) -> (
      try
        let Some (ty1,ty2, KR (Star,_))= proof_check ((a, BTVar kr) ::cxt) pf  r in
        Some (TyForall (a, kr,ty1), TyForall (a,kr,ty2),
              kr)
      with _ -> 
        (pr "error in branch CPForall in function proof_check";
         None
        )
    )

    |CPInst (pf,ty) -> (
      try
        let Some (TyForall(a1,(KR (k'',r'') as k1'),ty1'),
                  TyForall(a2,k2',ty2'),
                  KR(Star,r')) = proof_check cxt pf r in
        if k1'= k2' then
          let Some kr1 = kind_check cxt ty r''
          in
          if kr1 = k1' then
            Some (type_subst a1 ty1' ty,
                  type_subst a2 ty2' ty, KR(Star,r))
          else 
            (pr "error in branch CPInst in function proof_check";
            None)
        else 
          (pr "error in branch CPInst in function proof_check ";
           None)
      with _ -> 
        (pr "error in branch CPInst in function proof_check";
         None
        )
    )

let rec type_check (cxt : context)
    (term : fc_term) : fc_type option =  match term with
    | FCVar v -> (
        try
          let BTermVar ty'= List.assoc v cxt in
          Some ty'
        with
            Not_found -> 
              (pr "error in branch FCVar in function type_check";
               None)
    )
    | FCLam (v, ty, term') -> (
      try
        let Some ty' = type_check ((v, BTermVar ty) :: cxt) term' in
        Some (mk_arrow_ty ty ty')
      with _ -> 
        (pr "error in branch FCLam in function type_check";
         None)
    )
    | FCApp (t1, t2) -> (
      try
        let Some (TyApp ((TyApp ((TyConst TArrow), ty1)), ty2)) = type_check cxt t1 in
        let Some ty3 = type_check cxt t2 in
        if ty1 = ty3 then
          Some ty2
        else 
          (pr "error in branch FCApp in function type_check";
           None)
      with _ -> 
        (pr "error in branch FCApp in function type_check"; 
         None)
    )
    | FCTLam (a, kr, term') ->
      (try
         let Some ty1 =
           type_check ((a,BTVar kr)::cxt) term'
         in Some (TyForall (a,kr,ty1))
       with _ ->
         (pr "error in branch FCTLam in function type_check ";
         None)
      )
    | FCTApp (term', ty) ->(
      try 
        let Some (TyForall (a',KR(k',r'),ty')) =
          type_check cxt term' in
        let Some (KR (k'',r'')) =
          kind_check cxt ty r' in
        Some (type_subst a' ty' ty)
      with _ -> (
        pr "error in branch FCTApp in function type_check ";
        None)
     )
    | FCDatacon s ->
      (try
         let BDataCon (ty_cxt,ty) =
           List.assoc s cxt in
         Some (datacon_type ty_cxt ty)
       with _ -> 
         (
           pr s ;
           pr "error in branch FCDatacon in function type_check";
          None)
      )
    |FCCase (ty,term',branches) ->
       (try
         let branches = List.map (fun (Branch (a, b)) -> a, b) branches in
         let Some ty' = type_check cxt term' in
         let (head, args) = list_of_type ty' in
         let TyConst (Datatype name) = head in
         let consts = constructors cxt name in
         let check_branch (k, e) =
           let (ty_cxt, ty_k) = List.assoc k consts in
           let ty_k = type_subst_cxt ty_cxt args ty_k in
           let Some ty_e = type_check cxt e in
           let rec aux ty_k ty_e =
             (* We must check that both types have the same arguments *)
             begin match ty_k, ty_e with
               | TyApp (TyApp (TyConst TArrow, ty_k'), ty_k''),
                 TyApp (TyApp (TyConst TArrow, ty_e'), ty_e'') ->
                   ty_k' = ty_e'
               | _ -> ty_k = ty' && ty_e = ty
             end in
             aux ty_k ty_e in
           if List.for_all (fun (k,_) -> List.mem_assoc k branches) consts
             && List.for_all check_branch branches
           then Some ty
           else 
             (pr "error in branch FCCase in function type_check";
             None)
       with _ -> 
         (pr "error in branch FCCase in function type_check"; None))
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
             |_ -> 
               (pr "error in branch FCPLam in function type_check";
                None)
       with _ -> 
         (pr "error in branch FCPLam in function type_check";
         None)
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
         let Some (ty1,ty2, _ )= proof_check cxt proof Code  in
         if ty1 = tya &&
           ty2 = tyb
         then
           Some (sigma)
         else
           (pr "error in branch FCPApp in function type_check";
            None)
      with _ -> 
        (pr "error in branch FCPApp in function type_check";None)
    )
    | FCCoer (term',proof) -> (
      try
        let Some sigma1  = type_check cxt term' in
        let Some (sigma1', sigma2', KR(Star,Type))= proof_check cxt proof Type in

        if sigma1' = sigma1 then
          Some sigma2'
        else 
          (pr "error in branch FCCoer in function type_check";None)
      with _ -> 
        (pr "error in branch FCCoer in function type_check";None)
    )

