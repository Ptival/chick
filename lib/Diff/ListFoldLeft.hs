{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Diff.ListFoldLeft
  ( module Diff.ListFold
  , δListFoldLeft
  )where

import qualified Diff.List as DL
import Diff.ListFold

δListFoldLeft :: ΔListFold τ δτ a -> DL.Diff τ δτ -> a -> a
δListFoldLeft
  (ΔListFold
   { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  ) = go
  where
    go = \case
      DL.Insert  t δt -> go δt . onInsert t
      DL.Keep      δt -> go δt . onKeep
      DL.Modify  δ δt -> go δt . onModify δ
      DL.Permute p δt -> go δt . onPermute p
      DL.Remove    δt -> go δt . onRemove
      DL.Replace l    -> onReplace l
      DL.Same         -> onSame
