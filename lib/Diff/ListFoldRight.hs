{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Diff.ListFoldRight
  ( module Diff.ListFold
  , δListFoldRight
  ) where

import qualified Diff.List as DL
import Diff.ListFold
import Diff.Utils

δListFoldRight :: ∀ τ δτ a. ΔListFold τ δτ a -> [τ] -> DL.Diff τ δτ -> a -> a
δListFoldRight
  (ΔListFold
   { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame })
  = go
  where
    go :: [τ] -> DL.Diff τ δτ -> a -> a
    go l δl = case (δl, l) of
      (DL.Insert  t δt,     l') -> onInsert  t l . go l' δt
      (DL.Keep      δt, _ : l') -> onKeep      l . go l' δt
      (DL.Modify  δ δt, _ : l') -> onModify  δ l . go l' δt
      (DL.Permute p δt,   _   ) -> onPermute p l . go l' δt
        where l' = permute p l
      (DL.Remove    δt, _ : l') -> onRemove    l . go l' δt
      (DL.Replace r   ,   _   ) -> onReplace r l
      (DL.Same        ,   _   ) -> onSame      l
      _ -> error "δListFoldRight"
