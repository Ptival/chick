{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module Term.AlphaRenaming where

import Data.List

import Term.Free
import Term.Fresh
import Term.Term

αrename :: Variable -> Variable -> TermX ξ -> TermX ξ
αrename target replacement = go
  where
    go = \case
      Annot a t τ     -> Annot a (go t) (go τ)
      App   a t1 t2   -> App   a (go t1) (go t2)
      Hole  a         -> Hole  a
      Lam   a b t     -> goBound (Lam a) b t
      Let   a b t1 t2 -> goBound (\ b' t2' -> Let a b' (go t1) t2') b t2
      Pi    a b τ1 τ2 -> goBound (\ b' τ2' -> Pi  a b' (go τ1) τ2') b τ2
      Type  a         -> Type  a
      Var   a v
        | v == target -> Var a replacement
        | otherwise  -> Var a v
    goBound :: (Binder -> TermX ξ -> TermX ξ) -> Binder -> TermX ξ -> TermX ξ
    goBound k (Binder b) t = case b of
      Just v | v == target      -> k (Binder b) t
      Just v | v == replacement ->
               let f = freshAvoid [target, replacement] t in
               k (Binder (Just f)) (go (αrename v f t))
      _                        -> k (Binder b) (go t)

-- this is pretty inefficient
αrenameAvoidFree :: [Variable] -> TermX ξ -> TermX ξ
αrenameAvoidFree avoid term =
  -- we want to remove the following variables from the term
  let conflicts = avoid `intersect` freeVars term in
  go conflicts term
  where
    go []     t = t
    go (c:cs) t =
      go cs (αrename c (freshAvoid (c:cs) t) t)

-- this is pretty inefficient
{-
αrenameAvoidBound :: forall ξ. [Variable] -> TermX ξ -> TermX ξ
αrenameAvoidBound avoid term =
  let term' = αrenameAvoidFree avoid term in
  go term'
  where
    go :: TermX ξ -> TermX ξ
    go = \case
      Annot a t τ     -> Annot a (go t) (go τ)
      App   a t1 t2   -> App   a (go t1) (go t2)
      Hole  a         -> Hole  a
      Lam   a b t     -> let (b', t')  = goBound b t  in Lam a b' t'
      Let   a b t1 t2 -> let (b', t2') = goBound b t2 in Let a b' (go t1) t2'
      Pi    a b τ1 τ2 -> let (b', τ2') = goBound b τ2 in Pi  a b' (go τ1) τ2'
      Type  a         -> Type  a
      Var   a v       -> Var   a v
    goBound b@(Binder (Just v)) t
      | v `elem` avoid =
        let v' = freshAvoid avoid t in
        (Binder (Just v'), go (αrename v v' t))
      | otherwise      = (b, go t)
    goBound b t = (b, go t)
-}
