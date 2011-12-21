val kind_check :
  Syntax.context ->
  Syntax.fc_type -> Syntax.role -> Syntax.kind_and_role option
val type_cxt_check :
  Syntax.context ->
  Syntax.fc_type list -> ('a * Syntax.kind_and_role) list -> bool
val proof_check :
  Syntax.context ->
  Syntax.co_proof ->
  Syntax.role ->
  (Syntax.fc_type * Syntax.fc_type * Syntax.kind_and_role) option
val type_check : Syntax.context -> Syntax.fc_term -> Syntax.fc_type option
