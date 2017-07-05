{-# language FlexibleContexts #-}

module Diff.List
  ( Diff(..)
  , patch
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Exception

data Diff t dt
  = Same
  | Add    t  (Diff t dt)
  | Change dt (Diff t dt)
  | Flip      (Diff t dt)
  | Keep      (Diff t dt)
  | Remove    (Diff t dt)
  deriving (Show)

patch ::
  Member (Exc String) r =>
  (a -> da -> Eff r a) -> [a] -> Diff a da -> Eff r [a]
patch patchElem = go
  where
    go l d = case d of
      Same         -> return l
      Add     e d' -> go l d' >>= return . (e :)
      Change da d' -> case l of
        h : t -> do
          ph <- patchElem h da
          pt <- go t d'
          return $ ph : pt
        _     -> throwError "Change: empty list"
      Flip      d' -> case l of
        h1 : h2 : t -> go (h2 : h1 : t) d'
        _           -> throwError "Flip: < 2 elements list"
      Keep      d' -> case l of
        h : t -> go t d' >>= return . (h :)
        _     -> throwError "Keep: empty list"
      Remove    d' -> go (tail l) d'
