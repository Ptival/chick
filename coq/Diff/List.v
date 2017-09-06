Set Implicit Arguments.

From Coq Require Import
     List
     Unicode.Utf8
.

From Chick Require Import
     Notations
.

Import ListNotations.

Inductive Δ (τ : Type) (Δτ : Type) : Type :=
| Insert : ∀ (h  : τ)  (Δt : Δ τ Δτ), Δ τ Δτ
| Keep   : ∀ (Δt : Δ τ Δτ), Δ τ Δτ
| Modify : ∀ (Δh : Δτ) (Δt : Δ τ Δτ), Δ τ Δτ
| Same   : Δ τ Δτ
.
Arguments Same [τ Δτ].

Fixpoint patch τ Δτ (patchτ : Δτ -> τ -> option τ)
         (δ : Δ τ Δτ) : list τ -> option (list τ) :=
  match δ with

  | Insert h  δt => fun l => cons h <$> patch patchτ δt l

  | Keep δt =>
    fun l =>
      match l with
      | h :: t => cons h <$> patch patchτ δt t
      | _ => None
      end

  | Modify δh δt =>
    fun l =>
      match l with
      | []     => None
      | h :: t =>
        match (patchτ δh h, patch patchτ δt t) with
        | (Some h', Some t') => Some (h' :: t')
        | _ => None
        end
      end

  | Same         => Some

  end.
