From Chick Require Export
     LocalDeclaration
.

Record goal := Goal
  {   hypotheses : list localDeclaration
    ; conclusion : term
  }.

Notation "Γ ÷ φ" := (Goal Γ φ) (at level 50).
