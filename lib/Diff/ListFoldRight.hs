{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Diff.ListFoldRight
  ( module Diff.ListFold
  , δListFoldRight
  ) where

import qualified Diff.List as DL
import Diff.ListFold

δListFoldRight :: ΔListFold τ δτ a -> DL.Diff τ δτ -> a -> a
δListFoldRight
  (ΔListFold
   { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  ) = go
  where
    go = \case
      DL.Insert  t δt -> onInsert t  . go δt
      DL.Keep      δt -> onKeep      . go δt
      DL.Modify  δ δt -> onModify δ  . go δt
      DL.Permute p δt -> onPermute p . go δt
      DL.Remove    δt -> onRemove    . go δt
      DL.Replace l    -> onReplace l
      DL.Same         -> onSame
