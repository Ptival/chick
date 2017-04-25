From Coq Require Import
     EquivDec
     List
     Omega
     Operators_Properties
     Relation_Operators
     String
.

From Chick Require Import
     Goal
     LocalContext
     LocalDeclaration
     ReservedNotations
     TODO
.
Require Import Chick.Variable.

From Chick.CoreLtac.Syntax Require Import
     Atomic
     Tactic
.

From HaysTac Require Import HaysTac.

Import ListNotations.

Inductive fail := Fail : forall (l : nat), fail.

Inductive RV :=
| Fail__RV  : forall (l : fail), RV
| Goals__RV : forall (gs : goals), RV
| Value__RV : forall (v : value), RV
.

Coercion Fail__RV  : fail   >-> RV.
Coercion Goals__RV : goals  >-> RV.
Coercion Value__RV : value  >-> RV.

Inductive RVX :=
| Fail__RVX  : forall (l : fail), RVX
| Goals__RVX : forall (gs : goals), RVX
| Lam__RVX   : forall (l : lam), RVX
| Int__RVX   : forall (n : nat), RVX
.

Coercion Fail__RVX  : fail  >-> RVX.
Coercion Goals__RVX : goals >-> RVX.
Coercion Lam__RVX   : lam   >-> RVX.
Coercion Int__RVX   : nat     >-> RVX.

Definition coerce_RVX_to_RV (rvx : RVX) : RV :=
  match rvx with
  | Fail__RVX  n  => n
  | Goals__RVX gs => gs
  | Lam__RVX   l  => l
  | Int__RVX   n  => n
  end.

Coercion coerce_RVX_to_RV : RVX >-> RV.

Inductive RX :=
| Fail__RX  : forall (l : fail), RX
| Goals__RX : forall (gs : goals), RX
.

Coercion Fail__RX  : fail  >-> RX.
Coercion Goals__RX : goals >-> RX.

Definition coerce_RX_to_RVX (rx : RX) : RVX :=
  match rx with
  | Fail__RX  n  => n
  | Goals__RX gs => gs
  end.

Coercion coerce_RX_to_RVX : RX >-> RVX.

Definition values := list value.

Inductive RARGS :=
| Fail__RARGS : forall (l : fail), RARGS
| Vals__RARGS : forall (vs : values), RARGS
.

Coercion Fail__RARGS  : fail   >-> RARGS.
Coercion Vals__RARGS  : values >-> RARGS.
