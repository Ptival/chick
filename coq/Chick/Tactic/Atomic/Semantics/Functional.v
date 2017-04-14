From Coq Require Import
     DecidableClass
     List
.

From Chick Require Export
     Atomic
     Fresh
     Goal
     LocalContext
     LocalDeclaration
     Term
     Unify
.

From HaysTac Require Import HaysTac.

From SquiggleEq Require Export export.

Import ListNotations.
(*
Fixpoint piListRev pis φ :=
  match pis with
  | [] => φ
  | (pi :: pis) =>
    piListRev pis (mkPi None pi φ)
  end.

Definition piList pis φ := piListRev (List.rev pis) φ.

Reserved Notation "Γ ⊢ x ⇓ v" (at level 50).
*)

Definition isPi (t : term) :
  option { parts | let '(b, τ1, τ2) := parts in t = mkPi b τ1 τ2 } :=
  match t with
  | oterm (Pi true) [bterm [] τ1; bterm [v] τ2] =>
    Some (exist _ (Some v, τ1, τ2) eq_refl)
  | oterm (Pi false) [bterm [] τ1; bterm [] τ2] =>
    Some (exist _ (None, τ1, τ2) eq_refl)
  | _ => None
  end.

Open Scope bool.

Fixpoint atomic_exec (g : goal) (a : atomic) : option (list goal) :=
  let '(hyps ÷ concl) := g in
  match a with

    (*

| ApplyOK :
    forall H Γ τH τG pis,
      InLocalContext (LocalAssum H τH) Γ ->
      Unify τH (piList pis τG) ->
      Γ ÷ τG ⊢ AtomApply H ⇓ Some (List.map (Goal Γ) pis)

     *)

  | AtomApply H =>
    match
      List.find
        (fun d =>
           if variable_dec H (nameOfLocalDeclaration d)
           then isLocalAssum d
           else false)
        hyps
    with
    | Some (LocalAssum _ τH) => None (* TODO *)    | _ => None
    end

  | AtomAssumption =>
      if decide (TypeInLocalContext concl hyps)
      then Some []
      else None

  | AtomIntro x =>
    match isPi concl with
    | Some (exist _ (b, τ1, τ2) _) =>
      if decide (Fresh x hyps)
      then Some [(LocalAssum x τ1 :: hyps) ÷ τ2]
      else None
    | None => None
    end

  end
.

Notation "Γ ⊢ a ⇒ v" := (atomic_exec Γ a = Some v) (at level 50).
