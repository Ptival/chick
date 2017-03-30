From Coq Require Import List.
From Coq Require Import String.

From Chick Require Export term.

Record constructor : Type :=
  Constructor
    {   name      : string
      ; arguments : list (binder * term)
      ; indices   : list term
    }.

Definition onIndex i t := mkApp t i.

Definition onParam bp t := let '(b, p) := bp in mkPi b p t.

Definition onIndParam (bτ : (binder * term)) t :=
  let '(b, τ) := bτ in
  match b with
  | None   => mkApp t mkHole
  | Some v => mkApp t (mkVar v)
  end.

Definition
  constructorType
  (indName   : variable)
  (indParams : list (binder * term))
  (params    : list (binder * term))
  (indices   : list term)
  : term :=
  List.fold_right
    onParam
    (List.fold_right
       onIndex
       (List.fold_right
          onIndParam
          (mkVar indName)
          indParams
       )
       indices
    )
    params
.