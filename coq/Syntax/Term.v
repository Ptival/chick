Set Implicit Arguments.

From Coq Require Import
     Program
     String
     Unicode.Utf8
.

Definition Variable' := string.

Definition Binder ν := option ν.

Inductive Term (ν : Type) : Type :=
| App   : ∀ (t1 t2 : Term ν), Term ν
| Pi    : ∀ (τ1 : Term ν) (b : Binder ν) (τ2 : Term ν), Term ν
| Type' : Term ν
| Var   : ∀ (v : ν), Term ν
.
Arguments Type' [ν].

Definition VTerm ν := (ν * Term ν)%type.
Definition VTerms ν := list (VTerm ν).
Definition BTerm ν := (Binder ν * Term ν)%type.
Definition BTerms ν := list (BTerm ν).
