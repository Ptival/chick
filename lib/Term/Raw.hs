{-# language LambdaCase #-}
{-# language TypeFamilies #-}

module Term.Raw where

import Bound.Scope
import Data.Bifunctor

import Term.Term

type Raw = ()

type Term = TermX Raw
type Type = Term

raw :: TermX ξ ν -> Term ν
raw = \case
  Annot _ t τ    -> Annot () (raw t) (raw τ)
  App   _ t1 t2  -> App   () (raw t1) (raw t2)
  Hole  _        -> Hole  ()
  Lam   _ bt     -> Lam   () (hoistScope raw bt)
  Let   _ t1 bt2 -> Let   () (raw t1) (hoistScope raw bt2)
  Match _ d  bs  -> Match () (raw d)  (map (bimap id (hoistScope raw)) bs)
  Pi    _ τ1 bτ2 -> Pi    () (raw τ1) (hoistScope raw bτ2)
  Type  u        -> Type  u
  Var   _ v      -> Var   Nothing v
