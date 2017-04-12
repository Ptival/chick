From Chick Require Import
     LocalDeclaration
     Term
.

Definition localContext := list localDeclaration.

Definition InLocalContext (d : localDeclaration) (Γ : localContext) : Prop :=
  List.In d Γ.

Definition TypeInLocalContext (τ : term) (Γ : localContext) : Prop :=
  (exists v, InLocalContext (LocalAssum v τ) Γ)
  \/ (exists v t, InLocalContext (LocalDef v t τ) Γ).
