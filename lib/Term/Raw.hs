{-# language LambdaCase #-}
{-# language TypeFamilies #-}

module Term.Raw where

import Bound.Scope

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

raw :: TermX ξ ν -> Term ν
raw = \case
  Annot _ t τ    -> Annot () (raw t) (raw τ)
  App   _ t1 t2  -> App   () (raw t1) (raw t2)
  Hole  _        -> Hole  ()
  Lam   _ bt     -> Lam   () (hoistScope raw bt)
  Let   _ t1 bt2 -> Let   () (raw t1) (hoistScope raw bt2)
  Pi    _ τ1 bτ2 -> Pi    () (raw τ1) (hoistScope raw bτ2)
  Type  _        -> Type  ()
  Var     x      -> Var     x
