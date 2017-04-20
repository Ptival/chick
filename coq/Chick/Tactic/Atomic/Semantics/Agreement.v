From Coq Require Import
     DecidableClass
.

From Chick Require Import
     Atomic
.

From Chick.Tactic.Atomic.Semantics Require Import
     Functional
     Relational
.

From HaysTac Require Import HaysTac.

Theorem isPi_mkPi :
  forall b τ1 τ2,
  exists v, isPi (mkPi b τ1 τ2) = Some v.
Proof.
  intros.
  on binder destruct'; repeat eexists.
Qed.

Theorem agreement :
  forall Γ x v,
    Γ ⊢ x ⇓a Some v <-> Γ ⊢ x ⇒a v.
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
        do 2 on_head binder destruct'; on @eq now_inversion.
      }
      { contradiction. }
    }
    {
      simpl.
      admit. (* TODO *)
    }
  }
  {
    on_head goal destruct'.
    unfold atomic_exec in *.
    on_head atomic destruct'.
    {
      on @eq break_match_in.
      {
        do 3 on @eq break_let_pair_in.
        on @eq break_decide_in.
        {
          econstructor; eauto.
          congruence.
        }
        { discriminate. }
      }
      { discriminate. }
    }
    {
      on decide break_decide_in.
      {
        on @eq inversion'.
        now constructor.
      }
      { discriminate. }
    }
    { admit. (* TODO *) }
  }
Admitted.
