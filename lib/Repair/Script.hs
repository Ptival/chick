{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Repair.Script
  ( repair
  ) where

import           Control.Arrow
import           Control.Lens
-- import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Text.Printf

import qualified Diff.Atom as DA
import qualified Diff.GlobalDeclaration as DGD
-- import qualified Diff.GlobalEnvironment as DGE
import qualified Diff.List as DL
import qualified Diff.Script as DS
import qualified Diff.Term as DT
import           Diff.Utils
import qualified Diff.Vernacular as DV
-- import           PrettyPrinting.PrettyPrintableUnannotated
import           Repair.State
import qualified Repair.Term as RT
import           Script
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Term
import           Term.Variable
import qualified Typing.GlobalEnvironment as GE
-- import qualified Typing.LocalContext as LC
import           Utils
import           Vernacular

-- | `repairScript s δs` takes a script `s` and a script diff `δs`, and it computes a repaired
-- | script diff `δs'`, that propagates changes down the line
repair ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  Script Raw.Raw Variable -> DS.Diff Raw.Raw -> Eff r (DS.Diff Raw.Raw)
repair (Script s) δs =
  let exc reason = throwExc $ printf "Repair.Script/repair: %s" reason in
  case δs of

    -- even if the diff is same, we still might need to do some repair to account for changes in the
    -- global environment
    DL.Same ->
      case s of
        [] -> return DL.Same
        cmd : cmds ->
          case cmd of

            -- eventually, might want to update the name in case of collision?
            Definition n τ t -> do
              δτ <- RT.repair τ Type DT.Same
              δt <- RT.repair t τ    δτ
              withState
                (   over environment  (GE.addGlobalDef (Binder (Just n), t, τ))
                >>> over δenvironment (DL.Change (DGD.ChangeGlobalDef DA.Same δt δτ))
                ) $ do
                DL.Change (DV.ChangeDefinition DA.Same δτ δt) <$> repair (Script cmds) DL.Same

            Inductive _ind -> do
              -- I guess this one is weird:
              -- - it might be that the inductive type mentions a type that has been updated
              -- - it might be that constructor types mention a type that has been updated
              return DL.Same

    _ -> exc "TODO"
