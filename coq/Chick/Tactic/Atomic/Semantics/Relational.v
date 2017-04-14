From Coq Require Import List.

From Chick Require Export
     Atomic
     Fresh
     Goal
     LocalContext
     Term
     Unify
.

Import ListNotations.

Fixpoint piListRev pis τ2 :=
  match pis with
  | [] => τ2
  | (pi :: pis) =>
    piListRev pis (mkPi None pi τ2)
  end.

Definition piList pis φ := piListRev (List.rev pis) φ.

Reserved Notation "Γ ⊢ x ⇓ v" (at level 50).

Inductive AtomicExec : goal -> atomic -> option (list goal) -> Prop :=

| AssumptionOK :
    forall Γ φ,
      TypeInLocalContext φ Γ ->
      Γ ÷ φ ⊢ AtomAssumption ⇓ Some []

| IntroOK :
    forall b x Γ τ1 τ2 τG goals,
      Fresh x Γ ->
      τG = mkPi b τ1 τ2 ->
      goals = [(LocalAssum x τ1 :: Γ) ÷ τ2] ->
      Γ ÷ τG ⊢ AtomIntro x ⇓ Some goals

| ApplyOK :
    forall H Γ τH τG pis,
      InLocalContext (LocalAssum H τH) Γ ->
      Unify τH (piList pis τG) ->
      Γ ÷ τG ⊢ AtomApply H ⇓ Some (List.map (Goal Γ) pis)

where "Γ ⊢ x ⇓ v" := (AtomicExec Γ x v)
.
