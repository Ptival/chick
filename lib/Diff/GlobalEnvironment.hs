{-# language FlexibleContexts #-}
{-# language LambdaCase #-}

module Diff.GlobalEnvironment
  ( Diff
  , findGlobalDeclarationDiff
  , patch
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Text.Printf

import qualified Diff.Atom as DA
import qualified Diff.List as DL
import qualified Diff.GlobalDeclaration as DGD
import qualified Diff.Term as DT
import           Diff.Utils
import           Term.Variable
import           Typing.GlobalEnvironment
import           Typing.GlobalDeclaration

type Diff α = DL.Diff (GlobalDeclaration α Variable) (DGD.Diff α)

patch ::
  Member (Exc String) r =>
  GlobalEnvironment α Variable -> Diff α -> Eff r (GlobalEnvironment α Variable)
patch (GlobalEnvironment e) δe = do
  e' <- DL.patch DGD.patch e δe
  return $ GlobalEnvironment e'

findGlobalDeclarationDiff ::
  Member (Exc String) r =>
  Variable -> GlobalEnvironment α Variable -> Diff α -> Eff r (DT.Diff α)
findGlobalDeclarationDiff v e δe =
  let exc reason = throwExc $ printf "Diff.GlobalEnvironment/findGlobalDeclarationDiff: %s" reason in
  case δe of

    DL.Same -> return DT.Same

    DL.Insert ld δ ->
      if nameOf ld == v
      then exc "TODO: this might be DLD.Change, but could be we want to skip..."
      else findGlobalDeclarationDiff v e δ

    DL.Change DGD.Same δ -> findGlobalDeclarationDiff v e (DL.Keep δ)

    DL.Change (DGD.ChangeGlobalAssum δv δτ) δ ->
      case unGlobalEnvironment e of
        []    -> exc "DL.Change but empty environment"
        h : e' -> do
          v' <- DA.patch (nameOf h) δv
          if v' == v
          then return δτ
          else findGlobalDeclarationDiff v (GlobalEnvironment e') δ

    DL.Change (DGD.ChangeGlobalInd _δind) _δ -> exc "TODO: patch GlobalInd"

    DL.Permute _ _ -> exc "TODO: Permute"

    DL.Keep δ ->
      case unGlobalEnvironment e of
        []    -> exc "DL.Keep but empty context"
        h : e' -> do
          if nameOf h == v
          then return DT.Same
          else findGlobalDeclarationDiff v (GlobalEnvironment e') δ

    DL.Remove _ -> exc "TODO: Remove"
