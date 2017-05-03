{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Substitutable where

import Term.AlphaRenaming
import Term.Binder
import Term.Free
import Term.Fresh
import Term.Term
import Term.Variable

class Substitutable t where
  subst :: Variable -> TermX ξ -> t ξ -> t ξ

-- does NOT substitute in annotations!
instance Substitutable TermX where
  subst :: forall ξ. Variable -> TermX ξ -> TermX ξ -> TermX ξ
  subst target replacement = go
    where
      go :: TermX ξ -> TermX ξ
      go = \case
        Annot a t τ     -> Annot a (go t) (go τ)
        App   a t1 t2   -> App   a (go t1) (go t2)
        Hole  a         -> Hole  a
        Lam   a b t     -> goBound (Lam a) b t
        Let   a b t1 t2 -> goBound (\ b' t2' -> Let a b' (go t1) t2') b t2
        Pi    a b τ1 τ2 -> goBound (\ b' τ2' -> Pi  a b' (go τ1) τ2') b τ2
        Type  a         -> Type  a
        Var   a v
          | v == target -> replacement
          | otherwise  -> Var a v
      goBound :: (Binder -> TermX ξ -> TermX ξ) -> Binder -> TermX ξ -> TermX ξ
      goBound k (Binder b) t = case b of
        Just v | v == target      -> k (Binder b) t
        Just v | v `elem` freeVars replacement ->
                 let f = freshAvoid ([target] ++ freeVars replacement) t in
                 k (Binder (Just f)) (go (αrename v f t))
        _                        -> k (Binder b) (go t)
