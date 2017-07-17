From Coq Require Import List.

From Chick Require Export
     Fresh
     Goal
     LocalContext
     ReservedNotations
     Term
     Unify
.

From Chick.CoreLtac Require Import
     Syntax
.

Import ListNotations.

Fixpoint piListRev pis τ2 :=
  match pis with
  | [] => τ2
  | (pi :: pis) =>
    piListRev pis (mkPi None pi τ2)
  end.

Definition piList pis φ := piListRev (List.rev pis) φ.

Inductive AtomicExec : goal -> atomic -> option goals -> Prop :=

| Assumption__OK :
    forall Γ goal,
      ? \: goal ∈ Γ ->
      Γ ÷ goal ⊢ AtomAssumption ⇓a Some []

| Intro__OK :
    forall b x Γ τ1 τ2 goal goals,
      Fresh x Γ ->
      goal = mkPi b τ1 τ2 ->
      goals = [(LocalAssum x τ1 :: Γ) ÷ τ2] ->
      Γ ÷ goal ⊢ AtomIntro x ⇓a Some goals

| Apply__OK :
    forall H Γ τH goal pis goals,
      H \: τH ∈ Γ ->
      Unify τH (piList pis goal) ->
      goals = List.map (Goal Γ) pis ->
      Γ ÷ goal ⊢ AtomApply H ⇓a Some goals

| Assumption__FAIL :
    forall Γ goal,
      ~ (? \: goal ∈ Γ) ->
      Γ ÷ goal ⊢ AtomAssumption ⇓a None

| Intro__FAIL1 :
    forall Γ goal x,
      ~ Fresh x Γ ->
      Γ ÷ goal ⊢ AtomIntro x ⇓a None

| Intro__FAIL2 :
    forall Γ goal x,
      (forall b τ1 τ2, goal <> mkPi b τ1 τ2) ->
      Γ ÷ goal ⊢ AtomIntro x ⇓a None

| Apply__Fail1 :
    forall Γ goal H,
      Fresh H Γ ->
      Γ ÷ goal ⊢ AtomApply H ⇓a None

| Apply__Fail2 :
    forall Γ goal τH H,
      H \: τH ∈ Γ ->
      ~ Unify τH goal ->
      Γ ÷ goal ⊢ AtomApply H ⇓a None

where "Γ ⊢ x ⇓a v" := (AtomicExec Γ x v)

.
