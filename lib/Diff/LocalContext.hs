{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module Diff.LocalContext
  ( Diff
  , findLocalDeclarationDiff
  , patch
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Text.Printf

import qualified Diff.Atom as DA
import qualified Diff.List as DL
import qualified Diff.LocalDeclaration as DLD
-- import qualified Diff.Term as DT
import           Diff.Utils
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           Term.Variable
import           Typing.LocalContext
import           Typing.LocalDeclaration

type Diff α = DL.Diff (LocalDeclaration α Variable) (DLD.Diff α)

patch ::
  ( Member (Exc String) r
  , Member Trace r
  , Show α
  ) =>
  LocalContext α Variable -> Diff α -> Eff r (LocalContext α Variable)
patch (LocalContext γ) d = do
  γ' <- DL.patch DLD.patch γ d
  return $ LocalContext γ'

findLocalDeclarationDiff ::
  ( Member (Exc String) r
  , Member Trace r
  ) =>
  Variable -> LocalContext α Variable -> Diff α -> Eff r (DLD.Diff α)
findLocalDeclarationDiff v γ δγ =
  trace (
  printf
    "Diff.LocalContext/findLocalDeclarationDiff: Searching %s in %s"
    (prettyStr v) (prettyStrU γ)
  ) >>
  let exc (reason :: String) = throwExc $ printf "Diff.LocalContext/findLocalDeclarationDiff: %s" reason in
  case δγ of

    DL.Same ->
      case lookupType v γ of
        Nothing -> exc $ printf "Not found: %s" (show v)
        Just _  -> return DLD.Same

    DL.Insert ld δ ->
      if nameOf ld == v
      then exc "TODO: this might be DLD.Change, but could be we want to skip..."
      else findLocalDeclarationDiff v γ δ

    DL.Modify DLD.Same δ -> findLocalDeclarationDiff v γ (DL.Keep δ)

    DL.Modify dld@(DLD.Modify δv _δτ) δ ->
      case unLocalContext γ of
        []    -> exc "DL.Change but empty context"
        h : γ' -> do
          v' <- DA.patch (nameOf h) δv
          if v' == v
          then return dld
          else findLocalDeclarationDiff v (LocalContext γ') δ

    DL.Permute _ _ -> exc "TODO: Permute"

    DL.Keep δ ->
      case unLocalContext γ of
        []    -> exc "DL.Keep but empty context"
        h : γ' -> do
          if nameOf h == v
          then return DLD.Same
          else findLocalDeclarationDiff v (LocalContext γ') δ

    DL.Remove _ -> exc "TODO: Remove"

    DL.Replace _ -> exc "TODO: Replace"
