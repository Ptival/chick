{-# language FlexibleContexts #-}

module Diff.LocalContext
  ( Diff
  , patch
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Exception

import qualified Diff.List as DL
import qualified Diff.LocalDeclaration as DLD
import           Term.Variable
import           Typing.LocalContext
import           Typing.LocalDeclaration

type Diff α = DL.Diff (LocalDeclaration α Variable) (DLD.Diff α)

patch ::
  Member (Exc String) r =>
  LocalContext α Variable -> Diff α -> Eff r (LocalContext α Variable)
patch (LocalContext γ) d = do
  γ' <- DL.patch DLD.patch γ d
  return $ LocalContext γ'
