{-# language LambdaCase #-}
{-# language TypeFamilies #-}

module Term.Raw where

import Term.Term

data Raw

type Term = TermX Raw
type Type = Term

type instance X_Annot Raw = ()
type instance X_App   Raw = ()
type instance X_Hole  Raw = ()
type instance X_Lam   Raw = ()
type instance X_Let   Raw = ()
type instance X_Pi    Raw = ()
type instance X_Type  Raw = ()
type instance X_Var   Raw = ()

raw :: TermX ξ -> Term
raw = \case
  Annot _ t τ     -> Annot () (raw t) (raw τ)
  App   _ t1 t2   -> App   () (raw t1) (raw t2)
  Hole  _         -> Hole  ()
  Lam   _ n t     -> Lam   () n (raw t)
  Let   _ n t1 t2 -> Let   () n (raw t1) (raw t2)
  Pi    _ n τ1 τ2 -> Pi    () n (raw τ1) (raw τ2)
  Type  _         -> Type  ()
  Var   _ x       -> Var   () x
