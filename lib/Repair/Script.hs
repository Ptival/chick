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
import qualified Diff.Constructor as DC
import qualified Diff.GlobalDeclaration as DGD
import qualified Diff.GlobalEnvironment as DGE
import qualified Diff.Inductive as DI
import qualified Diff.List as DL
import qualified Diff.Script as DS
import qualified Diff.Term as DT
import           Diff.Utils
import qualified Diff.Vernacular as DV
import qualified Inductive.Constructor as C
import qualified Inductive.Inductive as I
import           PrettyPrinting.PrettyPrintableUnannotated
import           Repair.State
import qualified Repair.Term as RT
import qualified Repair.Vernacular as RV
import           Script
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Term
import           Term.Variable
import qualified Typing.GlobalEnvironment as GE
-- import qualified Typing.LocalContext as LC
import           Utils
import           Vernacular

vernacularDiffToGlobalEnvironmentDiff :: DV.Diff Raw.Raw -> DGE.Diff Raw.Raw -> DGE.Diff Raw.Raw
vernacularDiffToGlobalEnvironmentDiff = \case
  DV.ChangeDefinition δn δτ δt -> DL.Change (DGD.ChangeGlobalDef δn δτ δt)
  DV.ChangeInductive (DI.Change δn δps δis δcs) ->
    let δτ = DT.Same in
    DL.Change (DGD.ChangeGlobalAssum δn δτ)
  _ -> error "TODO: vernacularDiffToGlobalEnvironmentDiff"

-- | `withStateFromVernacular v δv` takes a vernacular command `v` and its (assumed repaired) diff `δv`
-- | and modifies the global environment to accound for the effect in the vernacular command before and
-- | after changes.
withStateFromVernacular ::
  ( Member (Exc String) r
  , Member (State RepairState) r
  , Member Trace r
  ) => Vernacular Raw.Raw Variable -> DV.Diff Raw.Raw -> Eff r a -> Eff r a
withStateFromVernacular v δv e =
  -- let exc (reason :: String) = throwExc $ printf "Repair.Script/withStateFromVernacular: %s" reason in
  case (v, δv) of

    (Definition n τ t, _) ->
      withState
      (over environment  (GE.addGlobalDef (Binder (Just n), τ, t))   >>>
       over δenvironment (vernacularDiffToGlobalEnvironmentDiff δv))
      $ e

    (Inductive (I.Inductive indName ps _is cs), DV.ChangeInductive (DI.Change δindName δps _ δcs)) ->
      withState
      (over environment  (GE.addGlobalAssum (Binder (Just indName), τind)) >>>
       over δenvironment (DL.Change (DGD.ChangeGlobalAssum δn δτind))
      ) $ do
      e
      -- let addConstructor (C.Constructor consName cps cis) =
      --       let τ = C.constructorRawType indName ps cps cis in
      --       over environment  (GE.addGlobalAssum (Binder (Just consName), τ))
      -- in
      -- let addConstructors = foldr (\ c acc -> addConstructor c . acc) id cs in
      -- do
      --   withState
      --     (addConstructors >>>
      --      over δenvironment (vernacularDiffToGlobalEnvironmentDiff δv)
      --     )
      --     $ e

-- | `repair s δs` takes a script `s` and a script diff `δs`, and it computes a repaired script diff
-- | `δs'`, that propagates changes down the line
repair ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  Script Raw.Raw Variable -> DS.Diff Raw.Raw -> Eff r (DS.Diff Raw.Raw)
repair (Script s) δs =
  trace (printf "Repair.Script/repair:\ns: %s\nδs: %s\n" (prettyStrU (Script s)) (show δs)) >>
  let exc (reason :: String) = throwExc $ printf "Repair.Script/repair: %s" reason in
  case (s, δs) of

    (v : s', DL.Change δv δs') -> do
      δv' <- RV.repair v δv
      withStateFromVernacular v δv' $
        DL.Change δv' <$> repair (Script s') δs'

    -- even if the diff is same, we still might need to do some repair to account for changes in the
    -- global environment
    ([], DL.Same) -> return DL.Same
    (cmd : cmds, DL.Same) ->
      case cmd of

        -- eventually, might want to update the name in case of collision?
        Definition n τ t -> do
          δτ <- RT.repair τ Type DT.Same
          δt <- RT.repair t τ    δτ
          withState
            (over environment  (GE.addGlobalDef (Binder (Just n), τ, t)) >>>
             over δenvironment (DL.Change (DGD.ChangeGlobalDef DA.Same δτ δt))
            ) $ do
            DL.Change (DV.ChangeDefinition DA.Same δτ δt) <$> repair (Script cmds) DL.Same

        Inductive _ind -> do
          -- I guess this one is weird:
          -- - it might be that the inductive type mentions a type that has been updated
          -- - it might be that constructor types mention a type that has been updated
          return DL.Same

    _ -> exc $ printf "TODO: %s" (show (s, δs))
