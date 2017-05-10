{-# language LambdaCase #-}

module Term.AlphaEquivalence where

import Term.AlphaRenaming
import Term.Binder
import Term.Free
import Term.Fresh
import Term.Term

{-
αeq :: TermX ξ -> TermX ψ -> Bool
αeq x y =
  case (x, y) of
  (Annot _ t τ, Annot _ t' τ') -> t `αeq` t' && τ `αeq` τ'
  (App _ t1 t2, App _ t1' t2') -> t1 `αeq` t1' && t2 `αeq` t2'
  (Hole _, _) -> True
  (_, Hole _) -> True
  (Lam _ b t, Lam _ b' t') -> αeqBound (b, t) (b', t')
  (Let _ b t1 t2, Let _ b' t1' t2') -> t1 `αeq` t1' && αeqBound (b, t2) (b', t2')
  (Pi _ b τ1 τ2, Pi _ b' τ1' τ2') -> τ1 `αeq` τ1' && αeqBound (b, τ2) (b', τ2')
  (Type _, Type _) -> True
  (Var _ v, Var _ v') -> v == v'
  (_, _) -> False
  where
    αeqBound :: (Binder, TermX ξ) -> (Binder, TermX ψ) -> Bool
    αeqBound (Binder b, t) (Binder b', t') = case (b, b') of
      (Nothing, Nothing) -> t `αeq` t'
      (Just v, Nothing)
        | isFree v t -> False
        | otherwise  -> t `αeq` t'
      (Nothing, Just v')
        | isFree v' t' -> False
        | otherwise    -> αeq t t'
      (Just v, Just v') ->
        let f = freshAvoid2 [v, v'] t t' in
        αrename v f t `αeq` αrename v' f t'
-}
