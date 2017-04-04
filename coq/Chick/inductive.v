From Coq Require Import List.
From Coq Require Import String.

From HaysTac Require Import HaysTac.

From Chick Require Export constructor.

Record inductive : Type :=
  Inductive'
    { name           : string
      ; parameters   : list (binder * term)
      ; indices      : list term
      ; constructors : list constructor
    }.

Definition onIndex i t := mkPi None i t.

Definition onParam '(b, p) t := mkPi b p t.

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
    (List.map (fun '(vars, body) => bterm vars body) branches).

Theorem nt_wf_mkMatch :
  forall i bs,
    List.Forall (fun b => nt_wf (snd b)) bs ->
    nt_wf (mkMatch i bs).
Proof.
  intros.
  constructor.
  - intros.
    on_head list induction'.
    + on In inversion'.
    + on Forall inversion'.
      on In inversion'.
      * on_head prod destruct'.
        constructor.
        assumption.
      * now find_apply.
  - admit.
Admitted.
