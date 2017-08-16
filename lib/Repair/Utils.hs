{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repair.Utils where

import           Control.Applicative
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Text.Printf

import qualified Diff.GlobalDeclaration as DGD
import qualified Diff.GlobalEnvironment as DGE
import qualified Diff.LocalContext as DLC
import qualified Diff.LocalDeclaration as DLD
import qualified Diff.Term as DT
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
  RepairState γ _ e _ <- get
  let res = LC.lookupType target γ <|> GE.lookupRawType target e
  case res of
    Nothing -> exc $ printf "Could not find %s in either local context or global environment" (prettyStr target)
    Just τ  -> return τ

findDeclarationDiff ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  Variable -> Eff r (Either (DLD.Diff Raw.Raw) (DGD.Diff Raw.Raw))
findDeclarationDiff v = do
  let exc (reason :: String) = throwExc $ printf "Repair.Utils/findDeclarationDiff: %s" reason
  RepairState γ δγ e δe <- get
  result <-
    (Left <$> DLC.findLocalDeclarationDiff  v γ δγ)
    `catchError`
    (\ (localError :: String) ->
        (Right <$> DGE.findGlobalDeclarationDiff v e δe)
        `catchError`
        (\ (globalError :: String) -> exc $ printf
          "Could not find %s in either local context or global environment:\n  > %s\n  > %s"
          (prettyStr v) localError globalError
        )
    )
  return result
