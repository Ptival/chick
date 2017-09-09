{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Diff.ListFoldLeft
  ( module Diff.ListFold
  , δListFoldLeft
  )where

import qualified Diff.List as DL
import Diff.ListFold
import Diff.Utils

δListFoldLeft :: ∀ τ δτ a. ΔListFold τ δτ a -> [τ] -> DL.Diff τ δτ -> a -> a
δListFoldLeft
  (ΔListFold
   { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame })
  = go
  where
    go :: [τ] -> DL.Diff τ δτ -> a -> a
    go l δl = case (δl, l) of
      (DL.Insert  t δt, l'    ) -> go l'  δt . onInsert  t l
      (DL.Keep      δt, _ : l') -> go l'  δt . onKeep      l
      (DL.Modify  δ δt, _ : l') -> go l'  δt . onModify  δ l
      (DL.Permute p δt, _     ) -> go l' δt  . onPermute p l
        where l' = permute p l
      (DL.Remove    δt, _ : l') -> go l'  δt . onRemove    l
      (DL.Replace r   , _)      ->             onReplace r l
      (DL.Same        , _)      ->             onSame      l
      _ -> error "δListFoldLeft"
