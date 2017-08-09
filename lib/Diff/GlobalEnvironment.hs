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

-- import qualified Diff.Atom as DA
import qualified Diff.List as DL
import qualified Diff.GlobalDeclaration as DGD
import qualified Diff.Term as DT
import           Diff.Utils
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import qualified Term.Raw as Raw
import           Term.Variable
import           Typing.GlobalEnvironment
import           Typing.GlobalDeclaration

type Diff α = DL.Diff (GlobalDeclaration α Variable) (DGD.Diff α)

patch ::
  ( Member (Exc String) r
  , Member Trace r
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
  Variable -> GlobalEnvironment Raw.Raw Variable -> Diff Raw.Raw -> Eff r (DT.Diff Raw.Raw)
findGlobalDeclarationDiff v e δe =
  -- trace (printf "Diff.GlobalEnvironment/findGlobalDeclarationDiff:\nSearching %s in %s" (prettyStr v) (prettyStrU e)) >>
  -- trace (printf "δe: %s" (show δe)) >>
  let exc (reason :: String) = throwExc $ printf "Diff.GlobalEnvironment/findGlobalDeclarationDiff: %s" reason in
  case δe of

    DL.Same ->
      case lookupRawType v e of
        Nothing -> exc $ printf "Not found: %s" (show v)
        Just _  -> return DT.Same

    DL.Insert ld δ ->
      if nameOf ld == v
      then exc "TODO: this might be DLD.Modify, but could be we want to skip..."
      else findGlobalDeclarationDiff v e δ

    DL.Modify DGD.Same δ -> findGlobalDeclarationDiff v e (DL.Keep δ)

    DL.Modify (DGD.ModifyGlobalAssum δv δτ) δ ->
      case unGlobalEnvironment e of
        []    -> exc "DL.Modify but empty environment"
        h : e' -> do
          -- v' <- DA.patch (nameOf h) δv
          if nameOf h == v --v' == v
          then return δτ
          else findGlobalDeclarationDiff v (GlobalEnvironment e') δ

    DL.Modify (DGD.ModifyGlobalDef δv δτ _) δ ->
      case unGlobalEnvironment e of
        []    -> exc "DL.Modify but empty environment"
        h : e' -> do
          -- v' <- DA.patch (nameOf h) δv
          if nameOf h == v --v' == v
          then return δτ
          else findGlobalDeclarationDiff v (GlobalEnvironment e') δ

    DL.Modify (DGD.ModifyGlobalInd _δind) _δ -> exc "TODO: patch GlobalInd"

    DL.Permute _ _ -> exc "TODO: Permute"

    DL.Keep δ ->
      case unGlobalEnvironment e of
        []    -> exc "DL.Keep but empty context"
        h : e' -> do
          if nameOf h == v
          then return DT.Same
          else findGlobalDeclarationDiff v (GlobalEnvironment e') δ

    DL.Remove _ -> exc "TODO: Remove"

    DL.Replace _ -> exc "TODO: Replace"
