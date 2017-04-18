From Coq Require Import
     EquivDec
     Omega (* because it messes with the notation *)
.

From Chick Require Export
     LocalDeclaration
.

From HaysTac Require Import HaysTac.

Record goal := Goal
  {   hypotheses : list localDeclaration
    ; conclusion : term
  }.

Notation "Γ ÷ φ" := (Goal Γ φ).

Global Instance EqDec_goal : EqDec goal eq.
Proof with decide_equality.
  intros [? ?] [? ?].
  on_head2 list destruct_EqDec...
  on_head2 term destruct_EqDec...
Defined.
