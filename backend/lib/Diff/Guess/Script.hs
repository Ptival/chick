{-# LANGUAGE FlexibleContexts #-}

module Diff.Guess.Script
  ( guess
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Trace

import qualified Diff.Guess.Vernacular as ΔGV
import qualified Diff.List as ΔL
import qualified Diff.Script as ΔS
import           Script
import qualified Term.Raw as Raw
import           Term.Variable

guess ::
  ( Member Trace r
  ) =>
  Script Raw.Raw Variable -> Script Raw.Raw Variable -> Eff r (ΔS.Diff Raw.Raw)
guess (Script (v1 : s1)) (Script (v2 : s2))
  | v1 == v2  = ΔL.Keep <$> guess (Script s1) (Script s2)
  | otherwise = do
      trace "########## v1 /= v2, making a guess"
      δv <- ΔGV.guess v1          v2
      δs <-     guess (Script s1) (Script s2)
      return $ ΔL.Modify δv δs
guess (Script []) (Script []) = return ΔL.Same
guess (Script []) (Script s2) = return $ ΔL.Replace s2
guess (Script _)  (Script []) = return $ ΔL.Replace []
