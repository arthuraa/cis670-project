open Syntax

let rec type_subst (var : string) (ty1 : fc_type) (ty2 : fc_type) =
  (* ty2 should be closed to avoid variable capture *)
  match ty1 with begin
    | TyVar var' ->
        if var = var' then ty2 else ty1
    | TyConst _ -> ty1
    | TyApp (ty11, ty12) -> TyApp (type_subst var ty11 ty2,
                                   type_subst var ty12 ty2)
    | TyForall (var', kr, ty1') ->
        if var = var'
        then ty1
        else TyForall (var', kr, type_subst var ty1' ty2)
  end

let sub_role r1 r2 =
  match r1, r2 with
    | (Code, Type) -> true
    | _ -> r1 = r2

let rec kind_check (cxt : context)

let rec check (cxt : context) (term : fc_term) : fc_type option =
  match term with
    | FCVar v -> begin
        try
          List.lookup v cxt
        with
            Not_found -> None
      end
    | FCLam (v, ty, term') ->
        check ((v, ty) : cxt) term'
    | FCApp (t1, t2) -> begin
        match check cxt t1, check cxt t2 with
          | (Some (TyApp (TyApp (TyConst TArrow) ty1) ty2),
             Some ty3) when ty1 = ty3 -> Some ty2
          | _ -> None
      end
    | FCTLam (ty, kr, term') ->
        check (BTVar (ty, kr) : cxt) term'
    | FCTApp (term', ty) ->
        match (check cxt term,


