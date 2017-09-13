{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Diff.ConcatMap
  ( module Diff.ListFold
  , δListConcatMap
  )where

import qualified Diff.List as DL
import           Diff.ListFold
import           Diff.ListFoldRight

δListConcatMap ::
  (τ -> [b]) ->
  (δτ -> τ -> Maybe τ) ->
  [τ] ->
  DL.Diff τ δτ ->
  Maybe (DL.Diff b δb)
δListConcatMap f patchA l δl =
  δListFoldRight (δListFoldConcatMap f patchA) l δl (Just DL.Same)
