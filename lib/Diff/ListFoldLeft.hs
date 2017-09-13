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
      (DL.Insert  t δt,     l') -> go l'  δt . onInsert  t   l'
      (DL.Keep      δt, e : l') -> go l'  δt . onKeep      e l'
      (DL.Modify  δ δt, e : l') -> go l'  δt . onModify  δ e l'
      (DL.Permute p δt,   _   ) -> go l' δt  . onPermute p   l -- here I want the original
        where l' = permute p l
      (DL.Remove    δt, e : l') -> go l'  δt . onRemove    e l'
      (DL.Replace r,        l') ->             onReplace r   l'
      (DL.Same,             l') ->             onSame        l'
      _ -> error "δListFoldLeft"
