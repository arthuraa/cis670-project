
%{
  open Fc_syntax
  open Util
  open Lexing 
  open Printf
  open Cxt (* for some table *)
%}

%token CODE TYPE FORALL
%token CASE 
%token DATA WHERE
%token NTH SYM NEWTYPE
%token TYPE FAMILY INSTANCE
%token LET 

%token <string>LIDENT 
%token <string>UIDENT 


%token STAR SLASH ARROW
%token LPAREN RPAREN
%token EOF 
%token DOT
%token  COLON
%token TILDE
%token LAMBDA
%token SEMI COMMA  LBRACKET RBRACKET 
%token FARROW

%token DOUBLECOLON
%token LTRI RTRI 
%token EQ
%token <int>INT
%token AT
%token TAPP
%left SEMI

%right ARROW 




%start input kind kind_and_role ty_def term proof ty_dec ty_def
%start clause 

%type <unit> input
%type <Fc_syntax.kind>kind
%type <Fc_syntax.kind_and_role>kind_and_role
%type <Fc_syntax.fc_type> ty_def 
%type <Fc_syntax.fc_term> term 
%type <Fc_syntax.co_proof> proof
%type <unit> ty_dec
%type <Fc_syntax.binder> clause 
%%


input: EOF {} |input_aux EOF {}
;

input_aux : 
  |ty_dec {}
  |term {}
  |input_aux ty_dec {}
  |input_aux term SEMI {}
;

kind: STAR {Star}
    | kind_and_role ARROW kind {KArrow ($1, $3)}
    | LPAREN kind RPAREN { $2 }
; 

kind_and_role: LPAREN kind RPAREN SLASH role {KR($2,$5)}
    | STAR SLASH role { KR(Star, ($3))  }
;



role: CODE { Code}
    | TYPE {Type}
;


/*
the normal way to do context sensitive grammars in yacc is
to overmatch and throw exceptions in the actions when the
match was wrong
*/


/*
data Maybe a where
  Just :: a -> Mabye a 
  Nothing :: Maybe a 

newtype Age = MkAge Int 
*/


ty_dec:  data_type_head clauses SEMI {
  let data_ctors = $2 in 
  List.iter (fun c -> cxt := c :: !cxt ) data_ctors
}
    | NEWTYPE UIDENT EQ UIDENT  ty_def  SEMI {
      let name = $2 in 
      let ty1 = TyConst (TFunction name) in 
      let ty2 = $5 in 
      let ctor_name = $4 in 
      begin
        Hashtbl.add new_type_tbl name true;
        cxt := (name, BTConst Star) :: !cxt ;
        cxt:= (ctor_name,
               BCoer([],TyEq(ty1,ty2),Type)) :: !cxt;
        
      end 
    }

    | TYPE FAMILY UIDENT ty_kr_list SEMI {
      begin
        Hashtbl.add function_tbl $3 (ref 0) ;
        let krs = List.map snd $4 in 
        let kind = list_of_krs krs in 
        cxt := ($3, BTConst kind) :: !cxt 
      end 
    }

    | TYPE INSTANCE LBRACKET ty_kr_list RBRACKET UIDENT 
        type_list EQ ty_def SEMI {
      let name = $6 in 
      let tys = $7 in 
      let type_cxt = $4 in 
      let ty2 = $9 in 
      try 
        let count_ref = Hashtbl.find function_tbl name in 
        let ty = TyConst (TFunction name) in 
        let ty1 = List.fold_left (fun x y -> TyApp(x,y)) ty tys in  
        cxt := ( "axiom" ^ name ^ (string_of_int !count_ref) , 
                BCoer(type_cxt, TyEq (ty1,ty2),Code)) :: !cxt ;
        incr count_ref 
      with Not_found 
          -> 
            let err_msg = name ^ "not found in type instance declaration" in 
            prerr_endline err_msg ; 
            raise Not_found
    }
    |TYPE INSTANCE UIDENT type_list EQ ty_def SEMI {
      let name = $3 in 
      let tys = $4 in 
      let type_cxt = [] in 
      let ty2 = $6 in 
      try 
        let count_ref = Hashtbl.find function_tbl name in 
        let ty = TyConst (TFunction name) in 
        let ty1 = List.fold_left (fun x y -> TyApp(x,y)) ty tys in  
        cxt := (name ^ (string_of_int !count_ref), 
                BCoer(type_cxt, TyEq (ty1,ty2),Code)) :: !cxt ;
        incr count_ref 
      with Not_found 
          -> 
            let err_msg = name ^ "not found in type instance declaration" in 
            prerr_endline err_msg ; 
            raise Not_found

    }

;

/* 
  split it so we can do some side effects here 
*/

data_type_head: DATA UIDENT ty_kr_list WHERE {
  begin 
    let type_ctor = $2 in 
    Hashtbl.add datatype_tbl type_ctor true;
    let krs = List.map snd $3 in 
    let kind = list_of_krs krs in 
    cxt := (type_ctor, BTConst kind) :: !cxt;
    (type_ctor, kind)
  end 
}
;

ty_kr_list:ty_kr_list_aux {List.rev $1 }
ty_kr_list_aux: /*empty*/ {[]}
    | ty_kr_list_aux LPAREN LIDENT COLON kind_and_role RPAREN  
        { ($3,$5):: $1 }
;


type_list: type_list_aux {List.rev $1 }
;
type_list_aux : /*empty*/ { [] }
    | type_list_aux ty_def { $2::$1}
;

tylist: tylist_aux {List.rev $1 }
; 
tylist_aux: /*empty*/ { [] }
    |  tylist LIDENT  { $2::$1 } 
;


clauses: LBRACKET clauses_aux RBRACKET {
  List.rev $2 }
;
clauses_aux : /*empty*/ { [] }
    | clauses_aux clause { $2::$1 }
;
clause: UIDENT DOUBLECOLON LBRACKET ty_kr_list RBRACKET ty_def
  {
   ($1,BDataCon($4, $6 ))   
  }
    | UIDENT DOUBLECOLON ty_def 
        {$1, BDataCon([],$3) }
;


ty_def: LIDENT { TyVar $1}
    | LPAREN ARROW RPAREN {TyConst TArrow}

    | LPAREN ty_def ARROW ty_def RPAREN 
        { TyApp(TyApp((TyConst TArrow),$2),$4) }

    | LPAREN TILDE kind RPAREN {TyConst (TEquality $3) }
    | LPAREN ty_def TILDE kind ty_def RPAREN FARROW ty_def
        {TyApp(TyApp(TyApp(TyConst(TEquality $4),$2),$5),$8)}
    | UIDENT 
        { let name = $1 in 
          try 
            let _ = Hashtbl.find datatype_tbl name in 
            TyConst (Datatype name)
          with Not_found -> 
            (try 
              let _ = Hashtbl.find function_tbl name in 
              TyConst (TFunction name)
            with Not_found -> 
              try 
                let _ = Hashtbl.find new_type_tbl name in 
                TyConst (TFunction name)
              with Not_found -> 
                (Parsing.(
                  let start_pos = rhs_start_pos 1 in
                  let end_pos = rhs_end_pos 1 in
                  let err_msg = sprintf
                    "%d.%d --- %d.%d: undefined type construtor %s"
                    start_pos.pos_lnum (start_pos.pos_cnum -start_pos.pos_bol)
                    end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol)
                    name in
                  prerr_endline err_msg ;
                  raise Not_found
                 ))
            )
        }

    | LPAREN ty_def  ty_def RPAREN  {TyApp ($2, $3) }
    | FORALL LIDENT COLON kind_and_role ty_def
        {TyForall($2,$4,$5)}

    | LPAREN ty_def RPAREN 
        {$2}
;

term: LIDENT {FCVar $1}
    | LAMBDA  LIDENT COLON ty_def  ARROW term 
        {FCLam ($2,$4,$6)}

    | LPAREN term  term RPAREN
        {FCApp($2, $3) }

    | LAMBDA  LIDENT COLON kind_and_role  ARROW term /* BLAMBDA */
        {FCTLam ($2,$4, $6)}

    | LPAREN term TAPP ty_def RPAREN
        {FCTApp($2,$4)}

    | UIDENT
        {
          let con_name = $1 in 
          try 
            let BDataCon _ = List.assoc con_name !cxt in 
            FCDatacon con_name 
          with 
              Not_found -> 
                prerr_endline ("undefined data ctor " ^ con_name);
                raise Not_found
        }

    | CASE LPAREN ty_def COMMA term RPAREN branches 
        {FCCase($3,$5,$7)}

    | LAMBDA  LIDENT COLON ty_def TILDE ty_def ARROW term 
      /* CLAMBDA */
        {FCPLam ($2,TyEq($4,$6),$8)}

    | LPAREN term AT proof RPAREN
        {FCPApp($2,$4)}
    | LPAREN term ARROW proof RPAREN /* BARROW */
        {FCCoer($2,$4)}

    | LPAREN term RPAREN
        {$2}
    | LET LIDENT EQ term /* Just for debugging purpuose */
        {
          let name = $2 in 
          let term = $4 in 
          Hashtbl.add term_tbl name (term,None) ;
          term
        }
 
    | LET LIDENT COLON ty_def EQ term 
        {
          let name = $2 in 
          let ty = $4 in 
          let term = $6 in 
          Hashtbl.add term_tbl name (term,Some ty);

          (match Check_term.type_check !Cxt.cxt term with 
              Some ty' when ty' = ty 
                       -> cxt := (name,BTermVar ty) :: !cxt 
            | None -> 
              prerr_endline 
                ( name ^ "does not type check during parsing") )
                ;
          term 
        }
;

branches: LBRACKET bs RBRACKET {List.rev $2}
bs: /*empty*/  {[]}
    | bs  UIDENT FARROW term SEMI  { Branch( $2,$4 ):: $1  }
;

proof: LIDENT {
  let proof = $1 in 
  try
    let BCoer (tys,_,_) = List.assoc proof !cxt  in 
    let tylist = List.map (fun (x,_) -> TyVar x) tys in 
    CPAssump (proof, tylist)
  with Not_found 
      -> (prerr_endline ("proof" ^ proof ^ "not found");
            raise Not_found 
      )
}
    | UIDENT {
  let proof = $1 in 
  try
    let BCoer (tys,_,_) = List.assoc proof !cxt  in 
    let tylist = List.map (fun (x,_) -> TyVar x) tys in 
    CPAssump (proof, tylist)
  with Not_found 
      -> (prerr_endline ("proof " ^ proof ^ " not found");
            raise Not_found 
      )
    }
    | LTRI ty_def RTRI {CPRefl $2}
    | LPAREN SYM proof RPAREN {CPSym $3}
    | LPAREN proof SEMI proof RPAREN {CPTrans($2,$4)}
    | LPAREN proof  proof RPAREN {CPApp ($2,$3)}
    | LPAREN NTH INT proof RPAREN {CPNth($3,$4)}
    | FORALL LPAREN LIDENT COLON kind_and_role RPAREN proof 
        {CPForall ($3,$5,$7)}
    | LPAREN proof AT ty_def RPAREN {CPInst($2,$4)}
;
