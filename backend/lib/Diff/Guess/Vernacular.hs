{-# LANGUAGE FlexibleContexts #-}

module Diff.Guess.Vernacular
  ( guess
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Trace

import qualified Diff.Guess.Atom as ΔGA
import qualified Diff.Guess.Inductive as ΔGI
import qualified Diff.Guess.Term as ΔGT
import qualified Diff.Vernacular as ΔV
import Term.Variable
import Vernacular

guess ::
  ( Member Trace r
  , Show α
  ) =>
  Vernacular α Variable -> Vernacular α Variable -> Eff r (ΔV.Diff α)
guess (Definition f1 n1 τ1 t1) (Definition f2 n2 τ2 t2) = do
  δf <- ΔGA.guess f1 f2
  δn <- ΔGA.guess n1 n2
  δτ <- ΔGT.guess τ1 τ2
  δt <- ΔGT.guess t1 t2
  return $ ΔV.ModifyDefinition δf δn δτ δt
guess (Inductive i1) (Inductive i2) = ΔV.ModifyInductive <$> ΔGI.guess i1 i2
guess _ v2 = return $ ΔV.Replace v2
