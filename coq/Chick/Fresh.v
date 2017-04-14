From Coq Require Import
     DecidableClass
     List
.

From Chick Require Import
     LocalContext
.
Require Import Chick.Variable.

Parameter Fresh : variable -> localContext -> Prop.
Parameter Decidable_Fresh : forall v Γ, Decidable (Fresh v Γ).
