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

From Chick.CoreLtac Require Import
     Results
     Semantics.Natural
     Semantics.SmallStepOperational.Untyped
     Semantics.Agreement.NaturalAndSmallStepOperationalUntyped.Atomic
     Syntax
     Syntax.SmallStepOperationalAbstractMachine
.

From HaysTac Require Import HaysTac.

Import ListNotations.

Local Ltac step := etransitivity; [ apply rt_step; reflexivity | simpl ].

Tactic Notation "on_context" open_constr(c) tactic(tac) :=
  match goal with [ H : context [c] |- _ ] => tac H end.

Theorem abstractMachineCorrect_val :
  forall (g : goal) (e : expr) (r : RV),
    g ⊢ e ↓v r ->
    forall s, E__v g e s ⇒* A__v s r.
Proof.
  do 4 intro.
  on_head ExprEval induction';  intros;
    try solve
        [ now step
        | step; find_rewrite_r; now step
        | step; break_match_in_goal; now subst
        ].
Qed.

Theorem abstractMachineCorrect_exec :
  (forall g  e  r, g  ⊢ e  ⇓    r -> forall s, E__ee  g  e  s ⇒* A__x s r)
  /\
  (forall g  t  r, g  ⊢ t  ↓x   r -> forall s, E__x   g  t  s ⇒* A__x s r)
  /\
  (forall gs es r, gs ⊢ es ↓seq r -> forall s, E__seq gs es s ⇒* A__x s r)
.
Proof.
  apply (
      Exec_ind
        (fun g  e  r => forall s, E__ee  g  e  s ⇒* A__x s r)
        (fun g  t  r => forall s, E__x   g  t  s ⇒* A__x s r)
        (fun gs es r => forall s, E__seq gs es s ⇒* A__x s r)
    );
    intros;
    try solve
        [ now step
        | step; find_rewrite_r; now step
        | step;
          on ExprEval ltac:(eapply_in_selected abstractMachineCorrect_val);
          on_head clos_refl_trans rewrite_r;
          now step
        | step; find_apply_in_hyp agreement; find_rewrite_r; now constructor
        | step; find_rewrite_r; step; find_rewrite_r; now step
        ].
  - step.
    find_rewrite_r.
    step.
    break_if_in_goal.
    + reflexivity.
    + congruence.
  - step.
    find_rewrite_r.
    step.
    break_match_in_goal; subst_all.
    + reflexivity.
    + break_match_in_goal; subst_all.
      * break_if_in_goal; subst_all.
        { congruence. }
        { reflexivity. }
      * reflexivity.
  - step.
    find_rewrite_r.
    step.
    break_if_in_goal; subst_all.
    + congruence.
    + reflexivity.
  - step.
    find_rewrite_r.
    step.
    break_if_in_goal; subst_all.
    + now find_rewrite_r.
    + congruence.
Qed.

Local Ltac next :=
  on clos_refl_trans inversion'; [ try solve [ on step_one inversion' ] | ].

Theorem next_configuration : forall (P : Prop) c c',
    c <> c' ->
    (stepConfiguration c ⇒* c' -> P) ->
    c ⇒* c' -> P.
Proof.
  intros.
  on_head clos_refl_trans
          ltac:(
    eapply_in_selected (
        clos_refl_trans_ind_right
          _ step_one (fun cInd => cInd = c -> P)
  )).
  - assumption.
  - congruence.
  - intros. subst. on_head step_one inversion'. auto.
  - reflexivity.
Qed.

Ltac forward :=
  on_head clos_refl_trans
          ltac:(fun H =>
                  eapply next_configuration in H;
                  [ eassumption
                  | discriminate
                  | simpl; clear H; intro
                  ]
               ).

Lemma A__x_Nil_steps_to_itself : forall c c',
    c ⇒* c' ->
    forall r,
    c = A__x Nil r ->
    c' = A__x Nil r.
Proof.
  do 4 intro.
  on clos_refl_trans induction'; intros; subst.
  - now on step_one inversion'.
  - reflexivity.
  - auto.
Qed.

Local Ltac step_A__x :=
  on_context (_ ⇒* _) ltac:(fun H => eapply A__x_Nil_steps_to_itself in H; eauto).

Notation "Γ ⊢ a ⇒ v" := (atomic_exec Γ a = Some v).

Ltac done :=
  find_eapply_in_hyp A__x_Nil_steps_to_itself; eauto; subst_all.

Theorem step_configuration : forall c1 c2,
    stepConfiguration c1 ⇒* c2 ->
    c1 ⇒* c2.
Proof.
  intros.
  eapply rt_trans.
  now apply rt_step.
  assumption.
Qed.

Theorem steps_exists_clos_refl_trans :
  forall (T X : Type) (R : T -> T -> Prop) a b (c : X -> T),
    (clos_refl_trans T R a b) ->
    (exists x, (clos_refl_trans T R b (c x))) ->
    exists x, (clos_refl_trans T R a (c x)).
Proof.
  intros.
  now setoid_rewrite H.
Qed.

Theorem step_exists_clos_refl_trans :
  forall (T X : Type) (R : T -> T -> Prop) a b (c : X -> T),
    R a b ->
    (exists x, (clos_refl_trans T R b (c x))) ->
    exists x, (clos_refl_trans T R a (c x)).
Proof.
  intros.
  eapply steps_exists_clos_refl_trans; eauto.
  now apply rt_step.
Qed.

Theorem estep_configuration : forall (R : Type) c1 c2,
    (exists r, stepConfiguration c1 ⇒* c2 r) ->
    exists r : R, c1 ⇒* c2 r.
Proof.
  intros.
  on @ex destruct'.
  eexists.
  etransitivity; [ | eassumption ].
  now constructor.
Qed.

Local Ltac estep := apply estep_configuration; simpl.

Local Ltac stack_from_goal :=
  match goal with
  | [ H : forall s : S__a, _ |- context [E__args _ _ ?s] ] => specialize (H s); destruct H
  | [ H : forall s : S__v, _ |- context [E__v    _ _ ?s] ] => specialize (H s); destruct H
  end.

Local Ltac step' := etransitivity; [ apply rt_step; reflexivity | simpl ].

Lemma abstractMachineCorrect :
  (forall (e : expr)   g r, E__ee g e Nil ⇒* A__x Nil r -> g ⊢ e ⇓  r)
  /\
  (forall (v : value)  g r, E__ee g v Nil ⇒* A__x Nil r -> g ⊢ v ↓v r)
  /\
  (forall (t : tactic) g r, E__x g t Nil ⇒* A__x Nil r -> g ⊢ t ↓x r).
Proof.
  eapply (
      ltac_ind
        (fun e => forall g r, E__ee g e Nil ⇒* A__x Nil r -> g ⊢ e ⇓  r)
        (fun v => forall g r, E__ee g v Nil ⇒* A__x Nil r -> g ⊢ v ↓v r)
        (fun t => forall g r, E__x  g t Nil ⇒* A__x Nil r -> g ⊢ t ↓x r)
    ); intros.
  {
    admit.
  }
  {
    admit.
  }
  {
    forward.
    forward.

    forward.
    break_match_in_hyp.
    { forward.
      break_match_in_hyp.
      { forward.
        done.
        eauto with NaturalSemantics.
      }
      { admit. }
      { admit. }
    }
    { admit. }
    { admit. }
    { admit. }
    { admit. }
  }
  { admit. }
  { admit. }
  { admit. }
  { admit. }
  { admit. }
  { admit. }
  { admit. }
  { admit. }
  { admit. }
  { admit. }
  { admit. }
  { admit. }
  { admit. }
  { admit. }
Admitted.
