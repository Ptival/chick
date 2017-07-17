From Coq Require Import
     List
     String
.

From Chick Require Export
     Term
.

Record constructor : Type :=
  Constructor
    {   name      : string
      ; arguments : list (binder * term)
      ; indices   : list term
    }.

Definition onIndex i t := mkApp t i.

Definition onParam '(b, p) t := mkPi b p t.

Definition onIndParam '(b, τ) t :=
  let τ := τ : term in (* to help the type-checker *)
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