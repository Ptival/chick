From Chick Require Export
     Term
.

Inductive localDeclaration :=
| LocalAssum : forall (v : variable) (τ : term),            localDeclaration
| LocalDef   : forall (v : variable) (t : term) (τ : term), localDeclaration
.

Definition nameOflocalDeclaration (ld : localDeclaration) : variable.
  destruct ld; exact v.
Defined.

Definition typeOflocalDeclaration (ld : localDeclaration) : term.
  destruct ld; exact τ.
Defined.
