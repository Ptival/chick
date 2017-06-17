{-# language LambdaCase #-}

module Term.Free where

import Data.Foldable
--import Data.List

import Term.Term
--import Term.Binder
--import Term.Variable

freeVars :: TermX ξ ν -> [ν]
freeVars = toList

isFree :: (Eq ν) => ν -> TermX ξ ν -> Bool
isFree v t = v `elem` freeVars t

{-
freeVars :: TermX ξ -> [Variable]
freeVars = go
  where
    go = \case
      Annot _ t τ    -> go t  `union` go τ
      App   _ t1 t2  -> go t1 `union` go t2
      Hole  _        -> []
      Lam   _ bt     -> goShadowed b t
      Let   _ t1 bt2 -> go t1 `union` goShadowed b t2
      Pi    _ τ1 bτ2 -> go τ1 `union` goShadowed b τ2
      Type  _        -> []
      Var   _ v      -> [v]
    goShadowed (Binder Nothing)  t = go t
    goShadowed (Binder (Just v)) t = go t \\ [v]

-- could be implemented in terms of `freeVars`, but this might be faster?
isFree :: Variable -> TermX ξ -> Bool
isFree target = go
  where
    go = \case
      Annot _ t τ     -> go t  || go τ
      App   _ t1 t2   -> go t1 || go t2
      Hole  _         -> False
      Lam   _ b t     -> goUnlessShadowed b t
      Let   _ b t1 t2 -> go t1 || goUnlessShadowed b t2
      Pi    _ b τ1 τ2 -> go τ1 || goUnlessShadowed b τ2
      Type  _         -> False
      Var   _ v       -> v == target
    goUnlessShadowed (Binder   Nothing) t = go t
    goUnlessShadowed (Binder (Just v)) t
      | v == target = False
      | otherwise  = go t
-}
