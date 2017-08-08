{-# language DeriveFoldable #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}

module Diff.Pair
  ( Diff(..)
  , patch
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Exception

data Diff δl δr
  = Same
  | Modify δl δr
  deriving (Show)

patch ::
  Member (Exc String) row =>
  (l -> δl -> Eff row l) ->
  (r -> δr -> Eff row r) ->
  (l, r) ->
  Diff δl δr ->
  Eff row (l, r)
patch patchL patchR (l, r) = \case
  Same         -> return (l, r)
  Modify δl δr -> (,) <$> patchL l δl <*> patchR r δr
