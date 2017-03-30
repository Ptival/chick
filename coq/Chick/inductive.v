From Coq Require Import List.
From Coq Require Import String.

From Chick Require Export constructor.

Record inductive : Type :=
  Inductive'
    { name           : string
      ; parameters   : list (binder * term)
      ; indices      : list term
      ; constructors : list constructor
    }.

Definition onIndex i t := mkPi None i t.

Definition onParam bp t := let '(b, p) := bp in mkPi b p t.

Definition
  inductiveType
  (params    : list (binder * term))
  (indices   : list term)
  (output    : term)
  : term :=
  List.fold_right
    onParam
    (List.fold_right
       onIndex
       output
       indices
    )
    params
.

Definition constructorBindings (c : constructor) : nat :=
  match c with
  | Constructor _ ps _ => List.length ps
  end.

Definition inductiveBindings (i : inductive) : list nat :=
  match i with
  | Inductive' _ _ _ cs => 0 :: List.map constructorBindings cs
  end.

Require Import SquiggleEq.export.

Definition
  mkMatch
  (i : inductive) (branches : list (list variable * term))
  : term :=
  oterm
    (Match (inductiveBindings i))
    (List.map (fun vb => let '(vars, body) := vb in bterm vars body) branches).
