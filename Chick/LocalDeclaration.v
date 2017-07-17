From Coq Require Import
     EquivDec
.

From Chick Require Export
     Term
.

From HaysTac Require Import HaysTac.

Inductive localDeclaration :=
| LocalAssum : forall (v : variable) (τ : term),            localDeclaration
| LocalDef   : forall (v : variable) (t : term) (τ : term), localDeclaration
.

Definition nameOfLocalDeclaration (ld : localDeclaration) : variable.
  destruct ld; exact v.
Defined.

Definition typeOfLocalDeclaration (ld : localDeclaration) : term.
  destruct ld; exact τ.
Defined.

Function isLocalAssum a := match a with LocalAssum _ _ => true | _ => false end.

Function isLocalDef a := match a with LocalDef _ _ _ => true | _ => false end.

Global Instance EqDec_goal : EqDec localDeclaration eq.
Proof with decide_equality.
  do 2 intro.
  do 2 on localDeclaration destruct'...
  - on_head2 variable destruct_EqDec;
      on_head2 term destruct_EqDec...
  - on_head2 variable destruct_EqDec...
    match goal with
    | [ |- context [ LocalDef _ ?x _ === LocalDef _ ?y _ ] ] =>
      destruct_EqDec x y
    end...
    match goal with
    | [ |- context [ LocalDef _ _ ?x === LocalDef _ _ ?y ] ] =>
      destruct_EqDec x y
    end...
Defined.
