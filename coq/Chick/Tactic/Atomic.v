From Coq Require Import List.
From Coq Require Import String.

From Chick Require Import
     Goal
     LocalContext
     LocalDeclaration
.
Require Import Chick.Variable.

Import ListNotations.

(*
Parameter formula : Type.
Parameter matcher : Type.
Parameter pattern : Type.
 *)

Inductive atomic : Type :=
| AtomIntro      : forall (x : variable), atomic
| AtomAssumption : atomic
| AtomApply      : forall (H : variable), atomic
(*| AtomAssert     : forall (x : variable) (φ : formula), atomic*)
.
