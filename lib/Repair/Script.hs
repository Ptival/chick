{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Repair.Script
  ( repair
  ) where

import           Control.Arrow
import           Control.Lens hiding (preview)
-- import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Text.Printf

import qualified Diff.Atom as DA
import qualified Diff.Constructor as DC
import           Diff.Eliminator
import qualified Diff.GlobalDeclaration as DGD
import qualified Diff.GlobalEnvironment as DGE
import qualified Diff.Inductive as DI
import qualified Diff.List as DL
import qualified Diff.Script as DS
import qualified Diff.Term as DT
import           Diff.Utils
import qualified Diff.Vernacular as DV
import           Inductive.Eliminator
import qualified Inductive.Inductive as I
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           Repair.State
import qualified Repair.Term as RT
import qualified Repair.Vernacular as RV
import           Script
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Term
import qualified Typing.GlobalEnvironment as GE
-- import qualified Typing.LocalContext as LC
import           Utils
import           Vernacular

vernacularDiffToGlobalEnvironmentDiff :: DV.Diff Raw.Raw -> DGE.Diff Raw.Raw -> DGE.Diff Raw.Raw
vernacularDiffToGlobalEnvironmentDiff = \case
  DV.ModifyDefinition δn δτ δt -> DL.Modify (DGD.ModifyGlobalDef δn δτ δt)
  DV.ModifyInductive (DI.Modify δn _δps _δis _δcs) ->
    let δτ = DT.Same in
    DL.Modify (DGD.ModifyGlobalAssum δn δτ)
  _ -> error "TODO: vernacularDiffToGlobalEnvironmentDiff"

withStateFromConstructors ::
  ( Member (Exc String) r
  , Member (State RepairState) r
  , Member Trace r
  ) =>
  DT.Diff Raw.Raw ->
  [I.Constructor Raw.Raw Variable] ->
  DL.Diff (I.Constructor Raw.Raw Variable) (DC.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withStateFromConstructors prefix l δl e =

  trace "ADDING A CONSTRUCTOR" >>
  traceState >>

  let todo (s :: String) = error $ printf "TODO: Repair.Script/wSFC %s" s in
  case (l, δl) of

    ([], DL.Same) -> e

    ([], _) -> todo "Empty list, other cases"

    (_c : _cs, DL.Insert  _  _)   -> todo "Insert"
    (_c : _cs, DL.Keep    _)      -> todo "Keep"

    ( c@(I.Constructor _ _consName cps cis) : cs,
      DL.Modify (DC.Modify δconsName δcps δcis) δcs) ->
      let δτc = DC.δconstructorRawType prefix cps δcps cis δcis in
      go c (DL.Modify (DGD.ModifyGlobalAssum δconsName δτc)) cs δcs

    (_c : _cs, DL.Modify _ _)     -> todo "Modify"
    (_c : _cs, DL.Permute _  _)   -> todo "Permute"
    (_c : _cs, DL.Remove  _)      -> todo "Remove"
    (_c : _cs, DL.Replace _)      -> todo "Replace"
    (c : cs, DL.Same) -> go c DL.Keep cs DL.Same

    where
    go c δge cs δcs =
      withState (over environment (GE.addConstructor c) >>> over δenvironment δge) $ do
      sanityCheck
      withStateFromConstructors prefix cs δcs $ do
        e

-- | `withStateFromVernacular v δv` takes a vernacular command `v` and its (assumed repaired) diff `δv`
-- | and modifies the global environment to accound for the effect in the vernacular command before and
-- | after changes.
withStateFromVernacular ::
  ( Member (Exc String) r
  , Member (State RepairState) r
  , Member Trace r
  ) => Vernacular Raw.Raw Variable -> DV.Diff Raw.Raw -> Eff r a -> Eff r a
withStateFromVernacular v δv e =

  let exc (reason :: String) =
        throwExc $ printf "Repair.Script/withStateFromVernacular: %s" reason
  in

  case (v, δv) of

  (Definition n τ t, _) ->
    withState
    (over environment  (GE.addGlobalDef (Binder (Just n), τ, t))   >>>
     over δenvironment (vernacularDiffToGlobalEnvironmentDiff δv))
    $ do
    sanityCheck
    e

  (Inductive ind@(I.Inductive indName ps is cs), DV.ModifyInductive δind@(DI.Modify δindName δps δis δcs)) -> do
    let τind = I.inductiveRawType ind
    let δτind = DI.δinductiveRawType (length ps) δps (length is) δis
    let prefix = DI.δinductiveRawConstructorPrefix δindName (length ps) δps
    δeliminatorType <- case δmkEliminatorType () ind δind of
      Nothing -> throwError "δmkEliminatorType failed"
      Just e -> return e
    trace (printf "PREFIX: %s" (show prefix))
    withState
      (over  environment (GE.addGlobalAssum (Binder (Just indName), τind)) >>>
       over δenvironment (DL.Modify (DGD.ModifyGlobalAssum δindName δτind))
      )
      $ withState
      (over  environment
       (GE.addGlobalAssum (Binder (Just (mkEliminatorName indName)),
                           mkEliminatorRawType ind
                           )) >>>
       over δenvironment
       (DL.Modify (DGD.ModifyGlobalAssum (δeliminatorName δind) δeliminatorType))
      ) $ do
      sanityCheck
      withStateFromConstructors prefix cs δcs e

  (Inductive ind, DV.ModifyInductive DI.Same) -> unchangedInductive ind
  (Inductive ind, DV.Same)                    -> unchangedInductive ind

  _ -> exc $ printf "TODO: %s" (show (v, δv))

  where
    unchangedInductive ind@(I.Inductive indName _ _ cs) = do
      let τind = I.inductiveRawType ind
      withState
        (over  environment (GE.addGlobalAssum (Binder (Just indName), τind)) >>>
         over δenvironment DL.Keep
        )
        $ withState
        (over  environment
         (GE.addGlobalAssum (Binder (Just (mkEliminatorName indName)),
                            mkEliminatorRawType ind
                            )) >>>
         over δenvironment DL.Keep
        ) $ do
        sanityCheck
        withStateFromConstructors DT.Same cs DL.Same e

-- | `repair s δs` takes a script `s` and a script diff `δs`, and it computes a repaired script diff
-- | `δs'`, that propagates changes down the line
repair ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  Script Raw.Raw Variable -> DS.Diff Raw.Raw -> Eff r (DS.Diff Raw.Raw)
repair script@(Script s) δs =

  trace (printf "Repair.Script/repair(s: %s, δs: %s)" (prettyStrU script) (prettyStr δs)) >>
  --sanityCheck >>
  traceState >>

  let exc (reason :: String) = throwExc $ printf "Repair.Script/repair: %s" reason in
  case (s, δs) of

    -- keeping, but still repairing
    (v : s', DL.Keep δs') -> do
      δv' <- RV.repair v DV.Same
      -- trace $ printf "Repair.Script/repair > δv' %s" (prettyStr δv')
      v' <- DV.patch v δv'
      trace $ printf "VERNAC BEFORE REPAIR: %s" (prettyStr v)
      trace $ printf "VERNAC  AFTER REPAIR: %s" (prettyStr v')
      withStateFromVernacular v δv' $
        DL.Modify δv' <$> repair (Script s') δs'

    (v : s', DL.Modify δv δs') -> do
      δv' <- RV.repair v δv
      -- trace $ printf "Repair.Script/repair > δv' %s" (prettyStr δv')
      v' <- DV.patch v δv'
      trace $ printf "VERNAC BEFORE REPAIR: %s" (prettyStr v)
      trace $ printf "VERNAC  AFTER REPAIR: %s" (prettyStr v')
      withStateFromVernacular v δv' $
        DL.Modify δv' <$> repair (Script s') δs'

    -- even if the diff is same, we still might need to do some repair to account for changes in the
    -- global environment
    ([], DL.Same) -> return DL.Same

    (cmd : cmds, DL.Same) ->
      case cmd of

        -- eventually, might want to update the name in case of collision?
        def@(Definition n τ t) -> do
          trace $ printf "*** Attempting to repair %s" (prettyStr def)
          δτ <- RT.repair τ Type DT.Same
          trace $ printf "*** Repaired type, δτ: %s" (prettyStr δτ)
          τ' <- DT.patch τ δτ
          trace $ printf "*** Repaired type, τ': %s" (prettyStr τ')
          δt <- RT.repair t τ    δτ
          trace $ printf "*** Repaired term, δt: %s" (prettyStr δt)
          t' <- DT.patch t δt
          trace $ printf "*** Repaired term, t': %s" (prettyStr t')
          withState
            (over  environment (GE.addGlobalDef (Binder (Just n), τ, t)) >>>
             over δenvironment (DL.Modify (DGD.ModifyGlobalDef DA.Same δτ δt))
            ) $ do
            trace "Repair.Script:180"
            sanityCheck
            DL.Modify (DV.ModifyDefinition DA.Same δτ δt) <$> repair (Script cmds) DL.Same

        Inductive _ind -> do
          -- I guess this one is weird:
          -- - it might be that the inductive type mentions a type that has been updated
          -- - it might be that constructor types mention a type that has been updated
          return DL.Same

    _ -> exc $ printf "TODO: %s" (show (s, δs))
