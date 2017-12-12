{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
import Prelude hiding (product)

import qualified Diff.Guess.Atom as ΔGA
import qualified Diff.Inductive as ΔI
import qualified Diff.List as ΔL
import Inductive.Inductive
import PrettyPrinting.Term ()
import Term.Variable

guess :: Inductive α Variable -> Inductive α Variable -> Eff r (ΔI.Diff α)
guess i1@(Inductive n1 ips1 iis1 u1 cs1) i2@(Inductive n2 ips2 iis2 u2 cs2) =
  if i1 == i2
  then return ΔI.Same
  else do
    δn   <- ΔGA.guess n1 n2
    δips <- return ΔL.Same -- TODO
    δiis <- return ΔL.Same -- TODO
    δu   <- ΔGA.guess u1 u2
    δcs  <- return ΔL.Same -- TODO
    return $ ΔI.Modify δn δips δiis δu δcs
