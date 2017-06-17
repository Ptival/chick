module Term.Raw where

import Prelude
import Bound.Scope (hoistScope)
import Term.Term (TermX(..))

type Raw = Unit

type Term = TermX Raw
type Type = Term

raw :: ∀ α ν. TermX α ν -> Term ν
raw = case _ of
  App   _ t1 t2  -> App   unit (raw t1) (raw t2)
  Hole  _        -> Hole  unit
  Lam   _ bt     -> Lam   unit (hoistScope raw bt)
  Let   _ t1 bt2 -> Let   unit (raw t1) (hoistScope raw bt2)
  Pi    _ τ1 bτ2 -> Pi    unit (raw τ1) (hoistScope raw bτ2)
  Type           -> Type
  Var   v        -> Var   v
