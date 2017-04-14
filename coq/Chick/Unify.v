From Coq Require Import
     DecidableClass
.

From Chick Require Import
     Term
.

Parameter Unify : term -> term -> Prop.
Parameter Decidable_Unify : forall t1 t2, Decidable (Unify t1 t2).
