From Coq Require Import String.

From HaysTac Require Import HaysTac.
From HaysTac Require Import Lists.List.

From Chick Require Export constructor.

Import ListNotations.

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

Definition branch : Type := list variable * term.
Definition branch_term '((vars, body) : branch) := bterm vars body.

Definition mkMatch (i : inductive) (discriminant : term) (branches : list branch)
  : term :=
  oterm
    (Match (inductiveBindings i))
    (bterm [] discriminant :: (List.map branch_term branches)).

Definition WellFormedBranch '(Constructor _ as' is') '((vs, t) : branch) : Prop :=
  List.length vs = List.length as' /\ nt_wf t.

Lemma WellFormedBranch_num_bvars : forall c b,
    WellFormedBranch c b -> num_bvars (branch_term b) = constructorBindings c.
Proof.
  intros.
  on constructor destruct'.
  on branch destruct'.
  on WellFormedBranch destruct'.
  assumption.
Qed.

Theorem nt_wf_mkMatch :
  forall i d bs,
    nt_wf d ->
    List.Forall (fun b => nt_wf (snd b)) bs ->
    List.Forall2 WellFormedBranch (constructors i) bs ->
    nt_wf (mkMatch i d bs).
Proof.
  intros.
  on inductive destruct'.
  simpl in *.
  constructor.
  - intros.
    on In inversion'.
    { now constructor. }
    on (list constructor) generalize_dependent'.
    on_head list induction'.
    { now on In inversion'. }
    on Forall inversion'.
    on In inversion'.
    * on_head prod destruct'.
      constructor.
      assumption.
    * intros.
      find_eapply; eauto.
  - simpl.
    f_equal.
    on (list constructor) generalize_dependent'.
    on_head list induction'.
    + intros.
      simpl in *.
      now on Forall2 inversion'.
    + intros.
      simpl in *.
      on_head Forall inversion'.
      on_head Forall2 inversion'.
      do 3 on @eq find_specialize_in.
      on @eq rewrite_r.
      simpl.
      f_equal.
      now apply WellFormedBranch_num_bvars.
Qed.
