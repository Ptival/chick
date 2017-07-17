From Coq Require Import
     DecidableClass
     EquivDec
     List
.

From Chick Require Export
     Fresh
     Goal
     LocalContext
     LocalDeclaration
     ReservedNotations
     Term
     TODO
     Unify
.

From Chick.CoreLtac.Syntax Require Import Atomic.

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

Fixpoint atomic_exec (g : goal) (a : atomic) : option goals :=
  let '(hyps ÷ concl) := g in
  match a with

  | AtomApply H =>
    match @witness _ (fun τ => H \: τ ∈ hyps) _ with
    | inl (exist _ τ IN) => TODO
    | inr _ => None
    end

  | AtomAssumption =>
      if decide (? \: concl ∈ hyps)
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

Notation "Γ ⊢ a ⇒a v" := (atomic_exec Γ a = v).
