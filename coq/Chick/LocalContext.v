From Coq Require Import
     DecidableClass
     EquivDec
.

From Chick Require Import
     LocalDeclaration
     Term
.

From HaysTac Require Import HaysTac.

Definition localContext := list localDeclaration.

(*
Definition InLocalContext (d : localDeclaration) (Γ : localContext) : Prop :=
  List.In d Γ.
 *)

Definition InLocalContextAssum H τ Γ :=
  List.In (LocalAssum H τ) Γ.

Notation "H \: τ ∈a Γ" :=
  (InLocalContextAssum H τ Γ)
  (at level 50).

Definition InLocalContextDef H τ Γ :=
  exists t, List.In (LocalDef H t τ) Γ.

Notation "H \: τ ∈d Γ" :=
  (InLocalContextDef H τ Γ)
  (at level 50).

Definition InLocalContext H τ Γ :=
  InLocalContextAssum H τ Γ \/ InLocalContextDef H τ Γ.

Notation "H \: τ ∈ Γ" :=
  (InLocalContext H τ Γ)
  (at level 50).

Definition TypeInLocalContext (τ : term) (Γ : localContext) : Prop :=
  exists H, H \: τ ∈ Γ.

Notation "? \: τ ∈ Γ" :=
  (TypeInLocalContext τ Γ)
  (at level 50).

Global Instance Decidable_TypeInLocalContext :
  forall τ Γ, Decidable (? \: τ ∈ Γ).
Proof.
Admitted.

Definition HypInLocalContext H (Γ : localContext) : Prop :=
  exists τ, (H \: τ ∈ Γ).

Notation "H \: ? ∈ Γ" :=
  (HypInLocalContext H Γ)
  (at level 50).

Global Instance Decidable_HypInLocalContext :
  forall H Γ, Decidable (H \: ? ∈ Γ).
Proof.
Admitted.

Class Witnesseable {T : Type} (P : T -> Prop) :=
  { witness : { t | P t } + not (exists t, P t)
  }.

Lemma nothing_in_empty_context:
  forall (H : variable) (x : term), (H \: x ∈ nil) -> False.
Proof.
  intros.
  on_head InLocalContext inversion'.
  - now on_head InLocalContextAssum inversion'.
  - now on_head InLocalContextDef inversion'.
Qed.

Lemma InLocalContext_cons:
  forall H a Γ x,
    H \: x ∈ Γ -> H \: x ∈ (a :: Γ).
Proof.
  intros.
  on_head InLocalContext inversion'.
  - left.
    unfold InLocalContextAssum in *.
    now auto with datatypes.
  - right.
    unfold InLocalContextDef in *.
    on @ex destruct'.
    eexists.
    eauto with datatypes.
Qed.

Global Instance Witnesseable_HypInLocalContext :
  forall H Γ, Witnesseable (fun τ => H \: τ ∈ Γ).
Proof.
  intros.
  constructor.
  on_head list induction'.
  { right.
    destruct 1.
    now eapply nothing_in_empty_context; eauto.
  }
  { on_head sum destruct'.
    { left.
      on_head sig destruct'.
      on_head term exists'.
      now apply InLocalContext_cons.
    }
    { on_head localDeclaration destruct'.
      { on2 variable destruct_EqDec.
        { left.
          on_head term exists'.
          repeat constructor.
          congruence.
        }
        { right.
          intro.
          on_head @ex destruct'.
          on_head InLocalContext inversion'.
          { find_apply_in_hyp List.in_inv.
            on or destruct'.
            { congruence. }
            { on not apply'.
              eexists.
              constructor.
              now find_apply.
            }
          }
          { unfold InLocalContextDef in *.
            on_head @ex destruct'.
            find_apply_in_hyp List.in_inv.
            on or destruct'.
            { congruence. }
            { on not apply'.
              eexists.
              constructor 2.
              eexists.
              now find_apply.
            }
          }
        }
      }
      { on_head2 variable destruct_EqDec.
        { left.
          on_head term exists'.
          constructor 2.
          on_head @equiv inversion'.
          econstructor.
          now eauto with datatypes.
        }
        { right.
          intro.
          on_head @ex destruct'.
          on_head InLocalContext inversion'.
          { find_apply_in_hyp List.in_inv.
            on or destruct'.
            { congruence. }
            { on not apply'.
              eexists.
              constructor.
              now find_apply.
            }
          }
          { unfold InLocalContextDef in *.
            on_head @ex destruct'.
            find_apply_in_hyp List.in_inv.
            on or destruct'.
            { congruence. }
            { on not apply'.
              eexists.
              constructor 2.
              eexists.
              now find_apply.
            }
          }
        }
      }
    }
  }
Qed.
