{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module Diff.GlobalEnvironment
  ( Diff
  , findGlobalDeclarationDiff
  , patch
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Text.Printf

import qualified Diff.List as DL
import qualified Diff.GlobalDeclaration as DGD
import           Diff.Utils
import           PrettyPrinting.PrettyPrintable
import qualified Term.Raw as Raw
import           Term.Variable
import           Typing.GlobalEnvironment
import           Typing.GlobalDeclaration

type Diff α = DL.Diff (GlobalDeclaration α Variable) (DGD.Diff α)

patch ::
  ( Member (Exc String) r
  , Member Trace r
  , PrettyPrintable α
  , Show α
  ) =>
  GlobalEnvironment α Variable -> Diff α -> Eff r (GlobalEnvironment α Variable)
patch (GlobalEnvironment e) δe = do
  e' <- DL.patch DGD.patch e δe
  return $ GlobalEnvironment e'

-- annotation has to be Raw because of inductives
findGlobalDeclarationDiff ::
  ( Member (Exc String) r
  , Member Trace r
  ) =>
  Variable -> GlobalEnvironment Raw.Raw Variable -> Diff Raw.Raw -> Eff r (DGD.Diff Raw.Raw)
findGlobalDeclarationDiff v e δe =
  -- trace (printf "Diff.GlobalEnvironment/findGlobalDeclarationDiff:\nSearching %s in %s" (prettyStr v) (prettyStrU e)) >>
  -- trace (printf "δe: %s" (prettyStr δe)) >>
  let exc (reason :: String) = throwExc $ printf "Diff.GlobalEnvironment/findGlobalDeclarationDiff: %s" reason in
  case δe of

    DL.Same ->
      case lookupRawType v e of
        Nothing -> exc $ printf "Not found: %s" (show v)
        Just _  -> return DGD.Same

    DL.Insert gd δ ->
      if nameOf gd == v
      then exc "TODO: this might be DGE.Modify, but could be we want to skip..."
      else findGlobalDeclarationDiff v e δ

    DL.Modify DGD.Same δ -> findGlobalDeclarationDiff v e (DL.Keep δ)

    DL.Modify dgd@(DGD.ModifyGlobalAssum _δv _δτ) δ ->
      case unGlobalEnvironment e of
        []    -> exc "DL.Modify but empty environment"
        h : e' -> do
          -- v' <- DA.patch (nameOf h) δv
          if nameOf h == v --v' == v
          then return dgd
          else findGlobalDeclarationDiff v (GlobalEnvironment e') δ

    DL.Modify dgd@(DGD.ModifyGlobalDef _δv _δτ _) δ ->
      case unGlobalEnvironment e of
        []    -> exc "DL.Modify but empty environment"
        h : e' -> do
          -- v' <- DA.patch (nameOf h) δv
          if nameOf h == v --v' == v
          then return dgd
          else findGlobalDeclarationDiff v (GlobalEnvironment e') δ

    DL.Modify (DGD.ModifyGlobalInd _δind) _δ -> exc "TODO: patch GlobalInd"

    DL.Permute _ _ -> exc "TODO: Permute"

    DL.Keep δ ->
      case unGlobalEnvironment e of
        []    -> exc "DL.Keep but empty context"
        h : e' -> do
          if nameOf h == v
          then return DGD.Same
          else findGlobalDeclarationDiff v (GlobalEnvironment e') δ

    DL.Remove _ -> exc "TODO: Remove"

    DL.Replace _ -> exc "TODO: Replace"
