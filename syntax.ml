type kind = Star | KArrow of kind_and_role * kind

and role = Code | Type

and kind_and_role = KR of kind * role

and type_constant =
    Datatype of string
  | TFunction of string
  | TArrow
  | TEquality of kind

and fc_type =
    TyVar of string
  | TyConst of type_constant
  | TyApp of fc_type * fc_type
  | TyForall of string * kind_and_role * fc_type

and co_proof =
    CPAssump of string * fc_type list
  | CPRefl of fc_type
  | CPSym of co_proof
  | CPTrans of co_proof * co_proof
  | CPApp of co_proof * co_proof
  | CPNth of int * co_proof
  | CPForall of string * kind_and_role * co_proof
  | CPInst of co_proof * fc_type

and eq_type =
    TyEq of fc_type * fc_type

and fc_term =
    FCVar of string
  | FCLam of string * fc_type * fc_term
  | FCApp of fc_term * fc_term
  | FCTLam of string * kind_and_role * fc_term
  | FCTApp of fc_term * fc_type
  | FCDatacon of string
  | FCCase of fc_type * fc_term * branch list
  | FCPLam of string * eq_type * fc_term
  | FCPApp of fc_term * co_proof
  | FCCoer of fc_term * co_proof

and branch =
    Branch of string * fc_term

and binder = bind_key  * bind_value 
and bind_key = string 
and bind_value = 
  | BTVar of  kind_and_role
  | BTConst of  kind
  | BCoer of type_context * eq_type * role
  | BTermVar of  fc_type
  | BDataCon of  type_context * fc_type

and context = binder list

and type_context = (string * kind_and_role) list

