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

module Diff.Guess.Inductive
  ( guess
  ) where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Exception
import Control.Monad.Freer.Trace
import Prelude hiding (product)

import qualified Diff.Guess.Atom as ΔGA
import qualified Diff.Guess.Constructor as ΔGC
import qualified Diff.Guess.Term as ΔGT
import qualified Diff.Atom as ΔA
import qualified Diff.Inductive as ΔI
import qualified Diff.List as ΔL
import qualified Diff.Term as ΔT
import qualified Diff.Triple as Δ3
import Inductive.Inductive
import PrettyPrinting.Term ()
import Term.Term

import StandardLibrary

guess ::
  ( Member Trace r
  , Show α
  ) =>
  Inductive α Variable -> Inductive α Variable -> Eff r (ΔI.Diff α)
guess i1@(Inductive n1 ips1 iis1 u1 cs1) i2@(Inductive n2 ips2 iis2 u2 cs2) =
  if i1 == i2
  then return ΔI.Same
  else do
    δn   <- ΔGA.guess n1 n2

{- To compute δips and δiis, I use a trick: instead of figuring out the
permutations from the list itself, I build the telescope term from those and ask
the term-diff-guesser to guess a diff for those terms.  From this term diff, I
extract the information to create the list diff.
-}

    let uniqueVar = Var Nothing "__UNIQUE__"

    let ipsTerm1 = quantifyInductiveParameters ips1 uniqueVar
    let ipsTerm2 = quantifyInductiveParameters ips2 uniqueVar
    δipsType <- ΔGT.guess ipsTerm1 ipsTerm2
    let δips = ΔGC.termDiffToListDiff ipsTerm1 δipsType
    trace "Guess for δips"
    trace $ show δips

    let iisTerm1 = quantifyInductiveIndices iis1 uniqueVar
    let iisTerm2 = quantifyInductiveIndices iis2 uniqueVar
    δiisType <- ΔGT.guess iisTerm1 iisTerm2
    let δiis = ΔGC.termDiffToListDiff iisTerm1 δiisType
    trace "Guess for δiis:"
    trace $ show δiis

    δu  <- ΔGA.guess u1 u2
    -- FIXME: for now I cheat and assume same number of constructors in same order
    δcsList <- mapM (uncurry ΔGC.guess) (zip cs1 cs2)
    let δcs = foldr ΔL.Modify ΔL.Same δcsList
    return $ ΔI.Modify δn δips δiis δu δcs

test = do
  δ <- runTrace $ guess indList indVec
  putStrLn $ show δ
