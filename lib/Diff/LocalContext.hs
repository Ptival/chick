{-# language FlexibleContexts #-}
{-# language LambdaCase #-}

module Diff.LocalContext
  ( Diff
  , findLocalDeclarationDiff
  , patch
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Text.Printf

import qualified Diff.Atom as DA
import qualified Diff.List as DL
import qualified Diff.LocalDeclaration as DLD
import qualified Diff.Term as DT
import           Diff.Utils
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

findLocalDeclarationDiff ::
  Member (Exc String) r =>
  Variable -> LocalContext α Variable -> Diff α -> Eff r (DT.Diff α)
findLocalDeclarationDiff v γ δγ =
  let exc reason = throwExc $ printf "Diff.LocalContext/findLocalDeclarationDiff: %s" reason in
  case δγ of

    DL.Same -> return DT.Same

    DL.Insert ld δ ->
      if nameOf ld == v
      then exc "TODO: this might be DLD.Change, but could be we want to skip..."
      else findLocalDeclarationDiff v γ δ

    DL.Change DLD.Same δ -> findLocalDeclarationDiff v γ (DL.Keep δ)

    DL.Change (DLD.Change δv δτ) δ ->
      case unLocalContext γ of
        []    -> exc "DL.Change but empty context"
        h : γ' -> do
          v' <- DA.patch (nameOf h) δv
          if v' == v
          then return δτ
          else findLocalDeclarationDiff v (LocalContext γ') δ

    DL.Permute _ _ -> exc "TODO: Permute"

    DL.Keep δ ->
      case unLocalContext γ of
        []    -> exc "DL.Keep but empty context"
        h : γ' -> do
          if nameOf h == v
          then return DT.Same
          else findLocalDeclarationDiff v (LocalContext γ') δ

    DL.Remove _ -> exc "TODO: Remove"
