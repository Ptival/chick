{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module Diff.Guess.Constructor
  ( guess
  , telescopeDiffToListDiff
  ) where

import           Control.Monad
import           Control.Monad.Freer
-- import Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Prelude hiding (product)
import           Text.Printf

import qualified Diff.Guess.Atom as ΔGA
import qualified Diff.Guess.Term as ΔGT
-- import qualified Diff.Atom as ΔA
import qualified Diff.Constructor as ΔC
-- import qualified Diff.Inductive as ΔI
import qualified Diff.List as ΔL
import qualified Diff.Pair as Δ2
import qualified Diff.Term as ΔT
import qualified Diff.Triple as Δ3
import           Inductive.Inductive
import           PrettyPrinting.Term ()
import           Term.Term
import qualified Term.Raw as Raw

{- This is quite specific.  It takes a diff for a telescope, and turns it into a
list of diffs for the parts of the telescope.  It should not concern itself with
things beyond the telescope, I think.
-}
telescopeDiffToListDiff ::
  Show α =>
  TermX α Variable -> ΔT.Diff α ->
  ΔL.Diff (α, Variable, TermX α Variable) (Δ3.Diff d e f)
telescopeDiffToListDiff = go
  where
    this :: String = "telescopeDiffToListDiff"
    go t δt = case (δt, t) of
      (ΔT.Same, _) -> ΔL.Same
      (ΔT.InsPi α δ1 b δ2, _) ->
        let v = case unBinder b of
              Nothing -> "_"
              Just v' -> v'
        in
        let τ = case ΔT.patchMaybe t δ1 of
              Nothing -> error $ printf "patch failed in %s" this
              Just t' -> t'
        in
        ΔL.Insert (α, v, τ) (go t δ2)
      (ΔT.CpyPi _ _ δ2, Pi _ _ bτ2) ->
        let (_, τ2) = unscopeTerm bτ2 in
        ΔL.Keep (go τ2 δ2)
      (ΔT.RemovePi δ2, Pi _ _ bτ2) ->
        let (_, τ2) = unscopeTerm bτ2 in
        ΔL.Remove (go τ2 δ2)
      (ΔT.Replace (Var _ _), _) -> ΔL.Replace []
      _ -> error $ printf "TODO %s: %s" this (show δt)

nestedApplicationsDiffToListDiff ::
  Show α =>
  TermX α Variable -> ΔT.Diff α ->
  ΔL.Diff (α, TermX α Variable) (Δ2.Diff d e)
nestedApplicationsDiffToListDiff = go
  where
    this :: String = "nestedApplicationsDiffToListDiff"
    go t δt = case (δt, t) of
      (ΔT.Replace (Var _ _), _) -> ΔL.Replace []
      (ΔT.Same, _) -> ΔL.Same
      _ -> error $ printf "TODO %s: %s" this (show δt)

guess ::
  ( Member Trace r
  ) =>
  Constructor Raw.Raw Variable -> Constructor Raw.Raw Variable ->
  Eff r (ΔC.Diff Raw.Raw)
guess c1@(Constructor _ n1 cps1 cis1) c2@(Constructor _ n2 cps2 cis2) =
  if c1 == c2
  then return ΔC.Same
  else do
    δn <- ΔGA.guess n1 n2

    let uniqueVar = Var Nothing "__UNIQUE__"

    let cpsTerm1 = quantifyConstructorParameters cps1 uniqueVar
    let cpsTerm2 = quantifyConstructorParameters cps2 uniqueVar
    δcpsType <- ΔGT.guess cpsTerm1 cpsTerm2
    let δcps = telescopeDiffToListDiff cpsTerm1 δcpsType
    trace "Guess for δcps"
    trace $ show δcps

    -- definitely a list diff-er for those
    let cisTerm1 = applyConstructorIndices cis1 uniqueVar
    let cisTerm2 = applyConstructorIndices cis2 uniqueVar
    δcisType <- ΔGT.guess cisTerm1 cisTerm2
    let δcis = nestedApplicationsDiffToListDiff cisTerm1 δcisType

    return $ ΔC.Modify δn δcps δcis
