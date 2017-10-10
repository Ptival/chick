{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module Diff.GlobalEnvironment
  ( Diff
  , findGlobalDeclarationDiff
  , findGlobalIndDiff
  , patch
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Text.Printf

import qualified Diff.List as DL
import qualified Diff.GlobalDeclaration as DGD
import           Diff.Utils
import           Inductive.Inductive
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
-- TODO: need to decide whether this find inductives or not, so far no?
findGlobalDeclarationDiff ::
  ( Member (Exc String) r
  , Member Trace r
  ) =>
  Variable -> GlobalEnvironment Raw.Raw Variable -> Diff Raw.Raw -> Eff r (DGD.Diff Raw.Raw)
findGlobalDeclarationDiff v e δe = do
  -- trace (printf "Diff.GlobalEnvironment/findGlobalDeclarationDiff:\nSearching %s in %s" (prettyStr v) (prettyStrU e))
  -- trace (printf "δe: %s" (prettyStr δe))
  let exc (reason :: String) =
        throwExc $ printf "Diff.GlobalEnvironment/findGlobalDeclarationDiff: %s" reason

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

    DL.Modify dgd δ ->
      case unGlobalEnvironment e of
        []    -> exc "DL.Modify but empty environment"
        h : e' -> do
          case dgd of

            DGD.ModifyGlobalAssum _δv _δτ ->
              if nameOf h == v --v' == v
              then return dgd
              else findGlobalDeclarationDiff v (GlobalEnvironment e') δ

            DGD.ModifyGlobalDef _δv _δτ _ ->
              if nameOf h == v --v' == v
              then return dgd
              else findGlobalDeclarationDiff v (GlobalEnvironment e') δ

            DGD.ModifyGlobalInd _δind ->
              findGlobalDeclarationDiff v (GlobalEnvironment e') δ

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

-- Assumption: the ind existed before, and we're just trying to find how it was
-- modified
findGlobalIndDiff ind e δe = do
  let exc (reason :: String) =
        throwExc $ printf "Diff.GlobalEnvironment/findGlobalIndDiff: %s" reason
  case (δe, unGlobalEnvironment e) of
    (DL.Insert _ δ, _) -> findGlobalIndDiff ind e δ
    (DL.Modify (DGD.ModifyGlobalInd δind) δ, GlobalInd ind' : e') ->
      if inductiveName ind' == inductiveName ind
      then return δind
      else findGlobalIndDiff ind (GlobalEnvironment e') δ
    (DL.Modify _ δ, _ : e') ->
      findGlobalIndDiff ind (GlobalEnvironment e') δ
    _ -> exc $ printf "Diff.GlobalEnvironment/findGlobalIndDiff: %s" (prettyStr δe)
