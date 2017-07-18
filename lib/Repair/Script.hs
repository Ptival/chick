{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Repair.Script where

import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Data.Foldable
import           Text.Printf

import qualified Diff.Atom as DA
import qualified Diff.List as DL
import qualified Diff.Script as DS
import qualified Diff.Term as DT
import qualified Diff.Vernacular as DV
import           PrettyPrinting.PrettyPrintableUnannotated
import qualified Repair.State as RS
import qualified Repair.Term as RT
import           Script
import           StandardLibrary
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Variable
import qualified Typing.GlobalEnvironment as GE
import qualified Typing.LocalContext as LC
import           Utils
import           Vernacular

-- | `repairScript s δs` takes a script `s` and a script diff `δs`, and it computes a repaired
-- | script diff `δs'`, that propagates changes down the line
repairScript ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RS.State) r
  ) =>
  Script Raw.Raw Variable -> DS.Diff Raw.Raw -> Eff r (DS.Diff Raw.Raw)
repairScript (Script s) δs =

  case δs of

    -- even if the diff is same, we still might need to do some repair to account for changes in the
    -- global environment
    DL.Same ->
      case s of
        [] -> return DL.Same
        cmd : cmds ->
          case cmd of

            Definition n τ t -> _

            Inductive ind -> _
