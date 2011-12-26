
%{
  open Fc_syntax
  open Lexing 
  open Printf
  let datatype_tbl = Hashtbl.create 30 
  let function_tbl = Hashtbl.create 30 

%}

%token CODE TYPE FORALL
%token CASE 
%token DATA WHERE
%token NTH SYM NEWTYPE
%token TYPE FAMILY INSTANCE

%token <string>LIDENT 
%token <string>UIDENT 


%token STAR SLASH ARROW
%token LPAREN RPAREN
%token EOF 
%token  COLON
%token TILDE
%token LAMBDA
%token SEMI COMMA  LBRACKET RBRACKET 


%token DOUBLECOLON
%token LTRI RTRI 
%token EQ
%token <int>INT
%token AT
%token TAPP
%left SEMI

%right ARROW 




%start input kind kind_and_role ty_def term proof ty_dec

%type <unit> input
%type <Fc_syntax.kind>kind
%type <Fc_syntax.kind_and_role>kind_and_role
%type <Fc_syntax.fc_type> ty_def 
%type <Fc_syntax.fc_term> term 
%type <Fc_syntax.co_proof> proof
%type <unit> ty_dec

%%


input: /*empty*/ {}
  |input  ty_dec {}
  |input  term {}
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
data Maybe a where
  Just :: a -> Mabye a 
  Nothing :: Maybe a 

newtype Age = MkAge Int 


*/


ty_dec: DATA UIDENT tylist WHERE clauses SEMI {}
    | NEWTYPE UIDENT EQ UIDENT type_list SEMI {}
    | TYPE FAMILY UIDENT tylist DOUBLECOLON STAR SEMI {}
    | TYPE INSTANCE UIDENT type_list EQ ty_def SEMI {}
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

clauses: LBRACKET clauses_aux RBRACKET {List.rev $2 }
;

clauses_aux : /*empty*/ { [] }
    | clauses clause { $2::$1 }
;

clause: UIDENT DOUBLECOLON ty_def {}
;


ty_def: LIDENT { TyVar $1}
    | LPAREN ARROW RPAREN {TyConst TArrow}
    | LPAREN TILDE kind RPAREN {TyConst (TEquality $3) }
    | UIDENT 
        { try 
            let _ = Hashtbl.find datatype_tbl $1 in 
            TyConst (Datatype $1)
          with Not_found -> 
            (try 
              let _ = Hashtbl.find function_tbl $1 in 
              TyConst (TFunction $1)
            with Not_found -> 
              Parsing.(
              let start_pos = rhs_start_pos 1 in 
              let end_pos = rhs_end_pos 1 in 
              printf "%d.%d --- %d.%d: undefined type construtor %s"
                start_pos.pos_lnum (start_pos.pos_cnum -start_pos.pos_bol)
                end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol)
                $1;
              raise Not_found
              ) )

             
                
        }

    | LPAREN ty_def  ty_def RPAREN  {TyApp ($2, $3) }
    | FORALL LIDENT COLON kind_and_role ty_def
        {TyForall($2,$4,$5)}

    | LPAREN ty_def RPAREN 
        {$2}
;

term: LIDENT {FCVar $1}
    | LAMBDA LPAREN LIDENT COLON ty_def RPAREN ARROW term 
        {FCLam ($3,$5,$8)}

    | LPAREN term  term RPAREN
        {FCApp($2, $3) }

    | LAMBDA LPAREN LIDENT COLON kind_and_role RPAREN ARROW term /* BLAMBDA */
        {FCTLam ($3,$5, $8)}

    | LPAREN term TAPP ty_def RPAREN
        {FCTApp($2,$4)}

    | UIDENT
        {FCDatacon $1}

    | CASE LPAREN ty_def COMMA term RPAREN branches 
        {FCCase($3,$5,$7)}

    | LAMBDA LPAREN LIDENT COLON ty_def TILDE ty_def ARROW term /* CLAMBDA */
        {FCPLam ($3,TyEq($5,$7),$9)}

    | LPAREN term AT proof RPAREN
        {FCPApp($2,$4)}
    | LPAREN term ARROW proof RPAREN /* BARROW */
        {FCCoer($2,$4)}

    | LPAREN term RPAREN
        {$2}

 
;

branches: LBRACKET bs RBRACKET {List.rev $2}
bs: /*empty*/  {[]}
    | bs SEMI UIDENT ARROW term  { Branch( $3,$5 ):: $1  }
;

proof: LIDENT { failwith "no implemented"}
    | LTRI ty_def RTRI {CPRefl $2}
    | LPAREN SYM proof RPAREN {CPSym $3}
    | LPAREN proof SEMI proof RPAREN {CPTrans($2,$4)}
    | LPAREN proof  proof RPAREN {CPApp ($2,$3)}
    | LPAREN NTH INT proof RPAREN {CPNth($3,$4)}
    | FORALL LPAREN LIDENT COLON kind_and_role RPAREN proof 
        {CPForall ($3,$5,$7)}
    | LPAREN proof AT ty_def RPAREN {CPInst($2,$4)}
;
