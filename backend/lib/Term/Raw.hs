module Term.Raw
  ( Raw,
    Term,
    Type,
    raw,
  )
where

import Bound.Scope (hoistScope)
import Control.Lens (over)
import Data.Bifunctor (first)
import Term.Term (TermX (..), scopedTerm)

type Raw = ()

type Term = TermX Raw

type Type = Term

raw :: TermX ξ ν -> Term ν
raw = \case
  Annot _ t τ -> Annot () (raw t) (raw τ)
  App _ t1 t2 -> App () (raw t1) (raw t2)
  Hole _ -> Hole ()
  Lam _ bt -> Lam () (over scopedTerm (hoistScope raw) bt)
  Let _ t1 bt2 -> Let () (raw t1) (over scopedTerm (hoistScope raw) bt2)
  Match _ d bs -> Match () (raw d) (map (first (const ())) bs)
  Pi _ τ1 bτ2 -> Pi () (raw τ1) (over scopedTerm (hoistScope raw) bτ2)
  Type u -> Type u
  Var _ v -> Var Nothing v
  UnsupportedOCaml o -> UnsupportedOCaml o
