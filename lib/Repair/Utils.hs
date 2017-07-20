{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repair.Utils where

import           Control.Applicative
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Text.Printf

import qualified Diff.GlobalEnvironment as DGE
import qualified Diff.LocalContext as DLC
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
  let exc (reason :: String) = throwExc $ printf "Diff.LocalContext/lookupType: %s" reason
  RepairState γ _ e _ <- get
  let res = LC.lookupType target γ <|> GE.lookupRawType target e
  case res of
    Nothing -> exc $ printf "Could not find %s in either local context or global environment" (prettyStr target)
    Just τ  -> return τ

findDeclarationDiff ::
  ( Member (Exc String) r
  , Member (State RepairState) r
  ) =>
  Variable -> Eff r (DT.Diff Raw.Raw)
findDeclarationDiff v = do
  let exc (reason :: String) = throwExc $ printf "Diff.LocalContext/findLocalDeclarationDiff: %s" reason
  RepairState γ δγ e δe <- get
  result <-
    DLC.findLocalDeclarationDiff  v γ δγ
    `catchError`
    (\ (_ :: String) -> DGE.findGlobalDeclarationDiff v e δe)
    `catchError`
    (\ (_ :: String) -> exc $ printf
                        "Could not find %s in either local context or global environment"
                        (prettyStr v)
    )
  return result
