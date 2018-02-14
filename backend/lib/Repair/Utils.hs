{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repair.Utils
  ( lookupType
  , findDeclarationDiff
  , findGlobalDeclarationDiff
  , unpackDeclarationDiff
  ) where

import           Control.Applicative
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Text.Printf

import qualified Diff.Atom as ΔA
import qualified Diff.Binder as ΔB
import qualified Diff.GlobalDeclaration as ΔGD
import qualified Diff.GlobalEnvironment as ΔGE
import qualified Diff.LocalContext as ΔLC
import qualified Diff.LocalDeclaration as ΔLD
import qualified Diff.Term as ΔT
-- import qualified Diff.Term as ΔT
import           Diff.Utils
import           PrettyPrinting.PrettyPrintable
import           Repair.State
import qualified Term.Raw as Raw
import           Term.Variable
import qualified Typing.GlobalEnvironment as GE
import qualified Typing.LocalContext as LC

lookupType ::
  ( Member (Exc String) r
  , Member (State RepairState) r
  ) => Variable -> Eff r (Raw.Type Variable)
lookupType target = do
  let exc (reason :: String) = throwExc $ printf "Repair.Utils/lookupType: %s" reason
  (γ, _) <- getContexts
  (e, _) <- getEnvironments
  let res = LC.lookupType target γ <|> GE.lookupRawType target e
  case res of
    Nothing -> exc $ printf "Could not find %s in either local context or global environment" (prettyStr target)
    Just τ  -> return τ

findGlobalDeclarationDiff ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  Variable -> Eff r (ΔGD.Diff Raw.Raw)
findGlobalDeclarationDiff v = do
  (e, δe) <- getEnvironments
  ΔGE.findGlobalDeclarationDiff v e δe

findDeclarationDiff ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  Variable -> Eff r (Either (ΔLD.Diff Raw.Raw) (ΔGD.Diff Raw.Raw))
findDeclarationDiff v = do
  let exc (reason :: String) = throwExc $ printf "Repair.Utils/findDeclarationDiff: %s" reason
  (γ, δγ) <- getContexts
  (e, δe) <- getEnvironments
  result <-
    (Left <$> ΔLC.findLocalDeclarationDiff  v γ δγ)
    `catchError`
    (\ (localError :: String) ->
        (Right <$> ΔGE.findGlobalDeclarationDiff v e δe)
        `catchError`
        (\ (globalError :: String) -> exc $ printf
          "Could not find %s in either local context or global environment:\n  > %s\n  > %s"
          (prettyStr v) localError globalError
        )
    )
  return result

unpackDeclarationDiff ::
  Either (ΔLD.Diff Raw.Raw) (ΔGD.Diff Raw.Raw) ->
  (ΔA.Diff Variable, ΔT.Diff Raw.Raw)
unpackDeclarationDiff = \case
  Left dld ->
    case dld of
      ΔLD.Same -> (ΔA.Same, ΔT.Same)
      ΔLD.ModifyLocalAssum δb δτv ->
        let δv = ΔB.toΔVariable δb in
        (δv, δτv)
      ΔLD.ModifyLocalDef δv δτv -> (δv, δτv)
  Right dgd ->case dgd of
    ΔGD.Same                       -> (ΔA.Same, ΔT.Same)
    ΔGD.ModifyGlobalAssum δv δτv   -> (δv, δτv)
    ΔGD.ModifyGlobalDef   δv δτv _ -> (δv, δτv)
    ΔGD.ModifyGlobalInd   _        -> error "TODO if this happens 2"
