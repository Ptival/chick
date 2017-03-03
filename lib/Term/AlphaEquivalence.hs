{-# language LambdaCase #-}

module Term.AlphaEquivalence where

import Term.AlphaRenaming
import Term.Free
import Term.Fresh
import Term.Term

αeq :: TermX ξ -> TermX ξ -> Bool
αeq x y = case (x, y) of
  (Annot _ t τ, Annot _ t' τ') -> αeq t t' && αeq τ τ'
  (App _ t1 t2, App _ t1' t2') -> αeq t1 t1' && αeq t2 t2'
  (Hole _, _) -> True
  (_, Hole _) -> True
  (Lam _ b t, Lam _ b' t') -> αeqBound (b, t) (b', t')
  (Let _ b t1 t2, Let _ b' t1' t2') -> αeq t1 t1' && αeqBound (b, t2) (b', t2')
  (Pi _ b τ1 τ2, Pi _ b' τ1' τ2') -> αeq τ1 τ1' && αeqBound (b, τ2) (b', τ2')
  (Type _, Type _) -> True
  (Var _ v, Var _ v') -> v == v'
  (_, _) -> False
  where
    αeqBound (Binder b, t) (Binder b', t') = case (b, b') of
      (Nothing, Nothing) -> αeq t t'
      (Just v, Nothing)
        | isFree v t -> False
        | otherwise  -> αeq t t'
      (Nothing, Just v')
        | isFree v' t' -> False
        | otherwise    -> αeq t t'
      (Just v, Just v') ->
        let f = fresh2 t t' in
        αeq (αrename v f t) (αrename v' f t')
