From Coq Require Import
     DecidableClass
.

From Chick.CoreLtac Require Import
     Semantics.Natural
     Semantics.SmallStepOperational.Untyped
     Syntax
.

From HaysTac Require Import HaysTac.

Theorem isPi_mkPi :
  forall b τ1 τ2,
  exists v, isPi (mkPi b τ1 τ2) = Some v.
Proof.
  intros.
  on binder destruct'; repeat eexists.
Qed.

Lemma isPi_None:
  forall conclusion : term,
    isPi conclusion = None ->
    forall (b : binder) (τ1 τ2 : term), conclusion <> mkPi b τ1 τ2.
Proof.
  intro.
  on_head term destruct'; intros.
  { on binder destruct'; simpl.
    { discriminate. }
    { discriminate. }
  }
  { on binder destruct';
      simpl in *;
      repeat (break_match_in_hyp; try discriminate). }
Qed.

Theorem agreement :
  forall g a v,
    g ⊢ a ⇓a v <-> g ⊢ a ⇒a v.
Proof.
  split; intros.
  {
    on AtomicExec inversion'.
    {
      simpl.
      match goal with
      | [ |- context [ decide ?P ] ] => now _decide_ P X
      end.
    }
    {
      simpl.
      pose proof isPi_mkPi.
      match goal with
      | [ |- context [mkPi ?b ?x ?y] ] => on @eq ltac:(fun H => specialize (H b x y))
      end.
      repeat on @ex destruct'.
      find_rewrite_r.
      repeat break_let_pair_in_goal.
      break_decide_in_goal.
      {
        do 2 on_head binder destruct'; now on @eq inversion'.
      }
      { contradiction. }
    }
    {
      simpl.
      admit. (* TODO *)
    }
    {
      admit.
    }
    {
      admit.
    }
    {
      admit.
    }
    {
      admit.
    }
    {
      admit.
    }
  }
  {
    on_head goal destruct'.
    unfold atomic_exec in *.
    on_head atomic destruct'.
    { on @eq break_match_in.
      { do 3 on @eq break_let_pair_in.
        break_decide_in_goal.
        { econstructor; eauto. }
        { now constructor. }
      }
      { subst_all.
        eapply Intro__FAIL2.
        now apply isPi_None. }
    }
    { break_decide_in_hyp; subst_all.
      { now constructor. }
      { now constructor. }
    }
    {
      break_match_in_hyp.
      { break_let_pair_in_hyp.
        admit. }
      { admit. }
    }
  }
Admitted.

Hint Resolve agreement : NaturalSemantics.
Hint Extern 0 (_ ⊢ _ ⇓a _) => eapply agreement : NaturalSemantics.
