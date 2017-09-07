{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Diff.ListFoldLeft where

import qualified Diff.List as DL

data ΔListFoldLeft τ δτ a = ΔListFoldLeft
  { onInsert  ::     τ -> a -> a
  , onKeep    ::          a -> a
  , onModify  ::    δτ -> a -> a
  , onPermute :: [Int] -> a -> a
  , onRemove  ::          a -> a
  , onReplace ::   [τ] -> a -> a
  , onSame    ::          a -> a
  }

δListFoldLeft :: ΔListFoldLeft τ δτ a -> DL.Diff τ δτ -> a -> a
δListFoldLeft
  (ΔListFoldLeft
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
