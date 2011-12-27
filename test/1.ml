
DATA Simple (a:*/T) WHERE {
  Toy :: { (a:*/T) } (Simple a)
}; 


DATA Bool WHERE {
  True :: Bool
  False :: Bool
}
;


DATA Nat WHERE {
  Z :: Nat
  S :: (Nat -> Nat)
} ;


DATA List (a:*/T) WHERE {
  Cons :: {(a:*/T)} (a -> ((List a) -> (List a)) )
  Nil :: {(a:*/T)} (List a)
};

DATA Rep (a:*/C) WHERE {
  Rint :: {(a:*/C)} (a ~ * Nat) => (Rep a)
  Rlist :: {(a:*/C)} (FORALL b :  */C 
    ( (a ~ * (List b )) => ((Rep b) -> (Rep a)) ) )            
};


DATA Tuple (a:*/T) WHERE{
  Tuple2 :: {(a:*/T)} (a -> (a -> (Tuple a )))
}
;

DATA Maybe (a:*/T) WHERE{
  Just :: {(a:*/T)} (a -> (Maybe a))
  Nothing :: {(a:*/T)} (Maybe a)
}
;

TYPE FAMILY F (a:*/C)
;

TYPE INSTANCE F Nat = Bool 
; 


TYPE INSTANCE { (a:*/C) } F (Maybe a) = (Tuple a)
;

NEWTYPE Age = MkAge Nat ; 



DATA Unit WHERE {
  U :: Unit 
}
;

LET mono_id : (Unit -> Unit) = \ x : Unit ->  x 
;

LET poly_id = \ a : */T -> (\ x : a -> x )
;


LET is_empty = \ x: (List Nat) -> CASE (Bool,x) {
  Nil => True ;
  Cons => \ x:Nat -> ( \ xs : (List Nat) -> False );
} ;


LET coer_age_nat = \x : (Age) -> (x -> MkAge);

LET coer_nat_age = \x : (Nat) -> (x -> (SYM MkAge));
