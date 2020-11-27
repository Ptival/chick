{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Diff.ConcatMap
  ( module Diff.ListFold,
    δconcatMap,
    δconcatMap',
  )
where

import qualified Diff.List as DL
import Diff.ListFold
  ( ΔListFold (..),
    δListFoldConcatMap,
    δListFoldConcatMap',
    δListFoldMkAppBinders,
    δListFoldMkAppTerms,
    δListFoldMkAppVariables,
    δListFoldMkPiGeneric,
    δListFoldMkPiGenericMaybe,
    δListFoldMkPiVariables,
  )
import Diff.ListFoldRight (δListFoldRight)

δconcatMap ::
  (τ -> [b]) ->
  (δτ -> τ -> Maybe τ) ->
  [τ] ->
  DL.Diff τ δτ ->
  Maybe (DL.Diff b δb)
δconcatMap f patchA l δl =
  δListFoldRight (δListFoldConcatMap f patchA) l δl (Just DL.Same)

δconcatMap' ::
  (τ -> [b]) ->
  (δτ -> τ -> Maybe (DL.Diff b δb -> DL.Diff b δb)) ->
  [τ] ->
  DL.Diff τ δτ ->
  Maybe (DL.Diff b δb)
δconcatMap' f patchBs l δl =
  δListFoldRight (δListFoldConcatMap' f patchBs) l δl (Just DL.Same)
