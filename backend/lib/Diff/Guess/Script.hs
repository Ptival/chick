{-# LANGUAGE FlexibleContexts #-}

module Diff.Guess.Script
  ( guess
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Trace

import qualified Diff.Guess.Vernacular as ΔGV
import qualified Diff.List as ΔL
import qualified Diff.Script as ΔS
import Script
import Term.Variable

guess ::
  ( Member Trace r
  , Show α
  ) =>
  Script α Variable -> Script α Variable -> Eff r (ΔS.Diff α)
guess (Script (v1 : s1)) (Script (v2 : s2))
  | v1 == v2  = ΔL.Keep <$> guess (Script s1) (Script s2)
  | otherwise = do
      δv <- ΔGV.guess v1 v2
      δs <-     guess (Script s1) (Script s2)
      return $ ΔL.Modify δv δs
guess (Script []) (Script []) = return ΔL.Same
guess (Script []) (Script s2) = return $ ΔL.Replace s2
guess (Script _)  (Script []) = return $ ΔL.Replace []
