{-# language LambdaCase #-}

module Term.AlphaEquivalence where

import Term.Term

αeq :: TermX ξ -> TermX ξ -> Bool
αeq t1 t2 = case (t1, t2) of
  (Annot _ t τ, Annot _ t' τ') -> αeq t t' && αeq τ τ'
  (App _ t1 t2, App _ t1' t2') -> αeq t1 t1' && αeq t2 t2'
  (Hole _, _) -> True
  (_, Hole _) -> True
  (_, _) -> error "TODO: αeq"
