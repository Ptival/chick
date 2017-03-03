{-# language LambdaCase #-}

module Term.Free where

import Data.List

import Term.Term

freeVars :: TermX ξ -> [Variable]
freeVars = go
  where
    go = \case
      Annot _ t τ     -> go t  `union` go τ
      App   _ t1 t2   -> go t1 `union` go t2
      Hole  _         -> []
      Lam   _ b t     -> goShadowed b t
      Let   _ b t1 t2 -> go t1 `union` goShadowed b t2
      Pi    _ b τ1 τ2 -> go τ1 `union` goShadowed b τ2
      Type  _         -> []
      Var   _ v       -> [v]
    goShadowed (Binder Nothing)  t = go t
    goShadowed (Binder (Just v)) t = go t \\ [v]

-- could be implemented in terms of `freeVars`, but this might be faster?
isFree :: Variable -> TermX ξ -> Bool
isFree v = go
  where
    go = \case
      Annot _ t τ     -> go t  && go τ
      App   _ t1 t2   -> go t1 && go t2
      Hole  _         -> True
      Lam   _ b t     -> goUnlessShadowed b t
      Let   _ b t1 t2 -> go t1 && goUnlessShadowed b t2
      Pi    _ b τ1 τ2 -> go τ1 && goUnlessShadowed b τ2
      Type  _         -> True
      Var   _ v'      -> not (v == v')
    goUnlessShadowed (Binder   Nothing) t = go t
    goUnlessShadowed (Binder (Just v')) t
      | v' == v    = True
      | otherwise = go t
