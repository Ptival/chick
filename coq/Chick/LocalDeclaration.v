From Chick Require Export
     Term
.

Inductive localDeclaration :=
| LocalAssum : forall (v : variable) (τ : term),            localDeclaration
| LocalDef   : forall (v : variable) (t : term) (τ : term), localDeclaration
.

Definition nameOfLocalDeclaration (ld : localDeclaration) : variable.
  destruct ld; exact v.
Defined.

Definition typeOfLocalDeclaration (ld : localDeclaration) : term.
  destruct ld; exact τ.
Defined.

Function isLocalAssum a := match a with LocalAssum _ _ => true | _ => false end.

Function isLocalDef a := match a with LocalDef _ _ _ => true | _ => false end.
