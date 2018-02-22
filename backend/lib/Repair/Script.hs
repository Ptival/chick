{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repair.Script
  ( repair
  , runRepair'
  ) where

import           Control.Arrow
import           Control.Monad (liftM)
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Text.Printf

import qualified Definition as D
import qualified DefinitionObjectKind as DOK
import qualified Diff.Atom as ΔA
import qualified Diff.Constructor as ΔC
import           Diff.Eliminator
import qualified Diff.GlobalDeclaration as ΔGD
import qualified Diff.GlobalEnvironment as ΔGE
import qualified Diff.Inductive as ΔI
import qualified Diff.List as ΔL
-- import qualified Diff.Pair as Δ2
import qualified Diff.Script as ΔS
import qualified Diff.Term as ΔT
-- import qualified Diff.Triple as Δ3
import           Diff.Utils
import qualified Diff.Vernacular as ΔV
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
import qualified Term.Universe as U
import           Typing.GlobalDeclaration
import           Vernacular

vernacularDiffToGlobalEnvironmentDiff ::
  ΔV.Diff Raw.Raw -> ΔGE.Diff Raw.Raw -> ΔGE.Diff Raw.Raw
vernacularDiffToGlobalEnvironmentDiff = \case
  ΔV.ModifyDefinition _ δn δτ δt -> ΔL.Modify (ΔGD.ModifyGlobalDef δn δτ δt)
  ΔV.ModifyInductive (ΔI.Modify δn _δps _δis _δu _δcs) ->
    let δτ = ΔT.Same in
    ΔL.Modify (ΔGD.ModifyGlobalAssum δn δτ)
  _ -> error "TODO: vernacularDiffToGlobalEnvironmentDiff"

withStateFromConstructors ::
  ( Member (Exc String) r
  , Member (State RepairState) r
  , Member Trace r
  ) =>
  ΔT.Diff Raw.Raw ->
  I.Φips Raw.Raw Variable -> ΔI.Δips Raw.Raw ->
  [I.Constructor Raw.Raw Variable] ->
  ΔL.Diff (I.Constructor Raw.Raw Variable) (ΔC.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withStateFromConstructors prefix ips δips cs δcs e =

  trace (printf "Adding Constructors: (%s, %s)" (preview cs) (preview δcs)) >>
  -- traceState >>

  let todo (s :: String) = error $ printf "TODO: Repair.Script/withStateFromConstructors %s" s in

  case (cs, δcs) of

    ([], ΔL.Same) -> e

    (_, ΔL.Insert c δcs') -> do
      let τc = I.constructorRawType True c
      trace $ printf "τc: %s" (preview τc)
      insert (ΔL.Insert (GlobalAssum (I.constructorName c) τc)) cs δcs'

    ([], _) -> todo $ printf "Empty list: %s" (show δcs)

    (_c : _cs, ΔL.Keep _) -> todo "Keep"

    (c@(I.Constructor _ _consName cps cis) : cs', ΔL.Modify δc δcs') ->
      case δc of
        ΔC.Modify δconsName δcps δcis ->
          let δτc = ΔI.δconstructorRawType prefix ips δips cps δcps cis δcis in
          modify c (ΔL.Modify (ΔGD.ModifyGlobalAssum δconsName δτc)) cs' δcs'
        ΔC.Same ->
          let δτc = ΔI.δconstructorRawType prefix ips δips cps ΔL.Same cis ΔL.Same in
          modify c (ΔL.Modify (ΔGD.ModifyGlobalAssum ΔA.Same δτc)) cs' δcs'

    (_c : _cs, ΔL.Permute _ _) -> todo "Permute"

    (_c : _cs, ΔL.Remove _) -> todo "Remove"

    (_c : _cs, ΔL.Replace _) -> todo "Replace"

    (c : cs', ΔL.Same) -> modify c ΔL.Keep cs' ΔL.Same

    where

      modify c δge cs δcs = do
        trace $ printf "About to add constructor modification: %s" (preview c)
        withConstructor c >>> withδGlobalEnv δge $ do
          sanityCheck
          withStateFromConstructors prefix ips δips cs δcs $ do
            e

      insert δge cs δcs = do
        trace $ printf "About to add constructor insertion"
        withδGlobalEnv δge $ do
          sanityCheck
          withStateFromConstructors prefix ips δips cs δcs $ do
            e

{- `withStateFromVernacular v δv` takes a vernacular command `v` and its
(assumed repaired) diff `δv` and modifies the global environment to accound for
the effect in the vernacular command before and after changes. -}
withStateFromVernacular ::
  ( Member (Exc String) r
  , Member (State RepairState) r
  , Member Trace r
  ) => Vernacular Raw.Raw Variable -> ΔV.Diff Raw.Raw -> Eff r a -> Eff r a
withStateFromVernacular v δv e = do

  trace $ printf "withStateFromVernacular (%s, %s)" (preview v) (preview δv)

  let exc (reason :: String) =
        throwExc $ printf "Repair.Script/withStateFromVernacular: %s" reason

  case (v, δv) of

    (Definition defn, _) ->
      withGlobalDef ( Binder (Just (D.definitionName defn))
                    , D.definitionType defn
                    , D.definitionTerm defn
                    )
      >>> withδGlobalEnv (vernacularDiffToGlobalEnvironmentDiff δv)
      $ do
      sanityCheck
      e

    (   Inductive ind@(I.Inductive indName ips iis _u cs)
      , ΔV.ModifyInductive δind@(ΔI.Modify δindName δips δiis _δu δcs)) -> do
      -- let τind = I.inductiveRawType ind
      let δτind = ΔI.δinductiveRawType (length ips) δips (length iis) δiis
      let prefix = ΔI.δinductiveRawConstructorPrefix δindName (length ips) δips
      δeliminatorType <- case δmkEliminatorType () ind δind of
        Nothing -> throwError "δmkEliminatorType failed"
        Just e -> return e
      trace (printf "PREFIX: %s" (show prefix))
      withGlobalInd ind δind $ do
        (env, _) <- getEnvironments
        trace $ printf "Environment after ind:\n%s" (prettyStrU env)
        withGlobalAssumIndType ind (δindName, δτind) $ do
            -- DO NOT MERGE WITH PREVIOUS withGlobalAssumIndType
            -- because mkEliminatorName needs the previous things in scope
            withGlobalAssumAndδ
              (Binder (Just (mkEliminatorName indName)), mkEliminatorRawType ind)
              (δeliminatorName δind,                     δeliminatorType)
              $ do
              sanityCheck
              withStateFromConstructors prefix ips δips cs δcs e

    (Inductive ind, ΔV.ModifyInductive ΔI.Same) -> unchangedInductive ind
    (Inductive ind, ΔV.Same)                    -> unchangedInductive ind

    _ -> exc $ printf "TODO: %s" (show (v, δv))

  where

    unchangedInductive ind@(I.Inductive indName ips _ _ cs) = do
      withGlobalInd ind ΔI.Same $ do
        withGlobalAssumIndType ind (ΔA.Same, ΔT.Same) $ do
          trace "UNCHANGED"
          sanityCheck
          withGlobalAssumAndδ
            (Binder (Just (mkEliminatorName indName)), mkEliminatorRawType ind)
            (ΔA.Same, ΔT.Same) $ do
            sanityCheck
            withStateFromConstructors ΔT.Same ips ΔL.Same cs ΔL.Same e

-- | `repair s δs` takes a script `s` and a script diff `δs`, and it computes a repaired script diff
-- | `δs'`, that propagates changes down the line
repair ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  Script Raw.Raw Variable -> ΔS.Diff Raw.Raw -> Eff r (ΔS.Diff Raw.Raw)
repair script@(Script s) δs =

  trace (printf "Repair.Script/repair(s: %s, δs: %s)" (prettyStrU script) (prettyStr δs)) >>
  --sanityCheck >>
  traceState >>

  let exc (reason :: String) = throwExc $ printf "Repair.Script/repair: %s" reason in

  case (s, δs) of

    (_, ΔL.Replace s') -> repair (Script s') ΔL.Same

    -- keeping, but still repairing
    (v : s', ΔL.Keep δs') -> do
      δv' <- RV.repair v ΔV.Same
      -- trace $ printf "Repair.Script/repair > δv' %s" (prettyStr δv')
      v' <- ΔV.patch v δv'
      trace $ printf "VERNAC BEFORE REPAIR: %s" (prettyStr v)
      trace $ printf "VERNAC  AFTER REPAIR: %s" (prettyStr v')
      withStateFromVernacular v δv' $
        ΔL.Modify δv' <$> repair (Script s') δs'

    (v : s', ΔL.Modify δv δs') -> do
      δv' <- RV.repair v δv
      -- trace $ printf "Repair.Script/repair > δv' %s" (prettyStr δv')
      v' <- ΔV.patch v δv'
      trace $ printf "VERNAC BEFORE REPAIR: %s" (prettyStr v)
      trace $ printf "VERNAC  AFTER REPAIR: %s" (prettyStr v')
      withStateFromVernacular v δv' $
        ΔL.Modify δv' <$> repair (Script s') δs'

    -- even if the diff is same, we still might need to do some repair to account for changes in the
    -- global environment
    ([], ΔL.Same) -> return ΔL.Same

    (v : s', ΔL.Same) -> do
      -- not sure why I was not calling RV.repair here, let's comment this
      -- and try the reasonable thing instead

      δv <- RV.repair v ΔV.Same
      withStateFromVernacular v δv $
        ΔL.Modify δv <$> repair (Script s') ΔL.Same

      --   Inductive _ind -> do
      --     -- I guess this one is weird:
      --     -- - it might be that the inductive type mentions a type that has been updated
      --     -- - it might be that constructor types mention a type that has been updated
      --     return ΔL.Same

    _ -> exc $ printf "TODO: %s" (show (s, δs))

runRepair' ::
  Script Raw.Raw Variable -> ΔS.Diff Raw.Raw ->
  Eff '[Trace] (Either String (Script Raw.Raw Variable))
runRepair' s δs = runAll repairThenPatch
  where
    runAll = runError
             . liftM fst
             . flip runState initialRepairState
    repairThenPatch = do
      δs' <- repair s δs
      trace $ printf "COMPUTED PATCH:\n\n%s\n\n" (show δs')
      ΔS.patch s δs'
