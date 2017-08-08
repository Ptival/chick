{-# language DeriveFoldable #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}

module Diff.List
  ( Diff(..)
  , patch
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Exception
import Text.Printf

import Diff.Utils

data Diff t δt
  = Same
  | Insert   t    (Diff t δt)
  | Modify  δt    (Diff t δt)
  | Permute [Int] (Diff t δt)
  | Keep          (Diff t δt)
  | Remove        (Diff t δt)
  | Replace [t]
  deriving (Show)

patch ::
  Member (Exc String) r =>
  (a -> da -> Eff r a) -> [a] -> Diff a da -> Eff r [a]
patch patchElem = go
  where
    failWith = throwExc . printf "[Diff.List.patch] %s"
    go l = \case

      Same -> return l

      Insert e δ -> go l δ >>= return . (e :)

      Modify δe δ -> case l of
        h : t -> do
          ph <- patchElem h δe
          pt <- go t δ
          return $ ph : pt
        _     -> failWith "Modify, empty list"

      Permute p δ ->
        let ll = length l in
        if ll > length p || ll < maximum p
        then failWith "Permut, permutation exceeds list size"
        else go (permute p l) δ

      Keep δ -> case l of
        h : t -> go t δ >>= return . (h :)
        _     -> failWith "Keep, empty list"

      Remove δ -> go (tail l) δ

      Replace r -> return r
