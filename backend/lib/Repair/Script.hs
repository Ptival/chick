{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Repair.Script
  ( repair,
    runRepair',
  )
where

import Control.Arrow ((>>>))
import qualified Definition as D
-- import qualified DefinitionObjectKind as DOK
import qualified Diff.Atom as ΔA
import qualified Diff.Constructor as ΔC
import Diff.Eliminator (δeliminatorName, δmkEliminatorType)
import qualified Diff.GlobalDeclaration as ΔGD
import qualified Diff.GlobalEnvironment as ΔGE
import qualified Diff.Inductive as ΔI
import qualified Diff.List as ΔL
-- import qualified Diff.Pair as Δ2
import qualified Diff.Script as ΔS
import qualified Diff.Term as ΔT
-- import qualified Diff.Triple as Δ3
import qualified Diff.Vernacular as ΔV
import Inductive.Eliminator
  ( mkEliminatorName,
    mkEliminatorRawType,
  )
import qualified Inductive.Inductive as I
import Language (Language (Chick))
import Polysemy (Member, Sem)
import Polysemy.Error (Error, runError, throw)
import Polysemy.State (State, runState)
import Polysemy.Trace (Trace, trace)
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyStr, preview),
  )
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (prettyStrU),
  )
import Repair.State
  ( RepairState,
    getEnvironments,
    initialRepairState,
    sanityCheck,
    traceState,
    withConstructor,
    withGlobalAssumAndδ,
    withGlobalAssumIndType,
    withGlobalDef,
    withGlobalInd,
    withδGlobalEnv,
  )
-- import qualified Repair.Term as RT
import qualified Repair.Vernacular as RV
import Script (Script (Script))
import Term.Binder (Binder (Binder))
import qualified Term.Raw as Raw
import Term.Variable (Variable)
import Text.Printf (printf)
-- import qualified Term.Universe as U
import Typing.GlobalDeclaration (GlobalDeclaration (GlobalAssum))
import Vernacular (Vernacular (Definition, Inductive))

vernacularDiffToGlobalEnvironmentDiff ::
  ΔV.Diff Raw.Raw -> ΔGE.Diff Raw.Raw -> ΔGE.Diff Raw.Raw
vernacularDiffToGlobalEnvironmentDiff = \case
  ΔV.ModifyDefinition _ δn δτ δt -> ΔL.Modify (ΔGD.ModifyGlobalDef δn δτ δt)
  -- FIXME: this was definitely wrong, do I rely on it?
  -- ΔV.ModifyInductive (ΔI.Modify δn _δps _δis _δu _δcs) ->
  --   let δτ = ΔT.Same in
  --   ΔL.Modify (ΔGD.ModifyGlobalAssum δn δτ)
  _ -> error "TODO: vernacularDiffToGlobalEnvironmentDiff"

withStateFromConstructors ::
  Member (Error String) r =>
  Member (State RepairState) r =>
  Member Trace r =>
  ΔT.Diff Raw.Raw ->
  I.Φips Raw.Raw Variable ->
  ΔI.Δips Raw.Raw ->
  [I.Constructor Raw.Raw Variable] ->
  ΔL.Diff (I.Constructor Raw.Raw Variable) (ΔC.Diff Raw.Raw) ->
  Sem r a ->
  Sem r a
withStateFromConstructors prefix ips δips cs δcs e =
  trace (printf "Adding Constructors: (%s, %s)" (preview @'Chick cs) (preview @'Chick δcs))
    >>
    -- traceState >>

    let todo (s :: String) = error $ printf "TODO: Repair.Script/withStateFromConstructors %s" s
     in case (cs, δcs) of
          ([], ΔL.Same) -> e
          (_, ΔL.Insert c δcs') -> do
            let τc = I.constructorRawType True c
            trace $ printf "τc: %s" (preview @'Chick τc)
            insert (ΔL.Insert (GlobalAssum (I.constructorName c) τc)) cs δcs'
          ([], _) -> todo $ printf "Empty list: %s" (show δcs)
          (_c : _cs, ΔL.Keep _) -> todo "Keep"
          (c@(I.Constructor _ _consName cps cis) : cs', ΔL.Modify δc δcs') ->
            case δc of
              ΔC.Modify δconsName δcps δcis ->
                let δτc = ΔI.δconstructorRawType prefix ips δips cps δcps cis δcis
                 in modify c (ΔL.Modify (ΔGD.ModifyGlobalAssum δconsName δτc)) cs' δcs'
              ΔC.Same ->
                let δτc = ΔI.δconstructorRawType prefix ips δips cps ΔL.Same cis ΔL.Same
                 in modify c (ΔL.Modify (ΔGD.ModifyGlobalAssum ΔA.Same δτc)) cs' δcs'
          (_c : _cs, ΔL.Permute _ _) -> todo "Permute"
          (_c : _cs, ΔL.Remove _) -> todo "Remove"
          (_c : _cs, ΔL.Replace _) -> todo "Replace"
          (c : cs', ΔL.Same) -> modify c ΔL.Keep cs' ΔL.Same
  where
    modify c δge cs δcs = do
      trace $ printf "About to add constructor modification: %s" (preview @'Chick c)
      withConstructor c >>> withδGlobalEnv δge $ do
        sanityCheck
        withStateFromConstructors prefix ips δips cs δcs e

    insert δge cs δcs = do
      trace $ printf "About to add constructor insertion"
      withδGlobalEnv δge $ do
        sanityCheck
        withStateFromConstructors prefix ips δips cs δcs e

{- `withStateFromVernacular v δv` takes a vernacular command `v` and its
(assumed repaired) diff `δv` and modifies the global environment to accound for
the effect in the vernacular command before and after changes. -}
withStateFromVernacular ::
  Member (Error String) r =>
  Member (State RepairState) r =>
  Member Trace r =>
  Vernacular Raw.Raw Variable ->
  ΔV.Diff Raw.Raw ->
  Sem r a ->
  Sem r a
withStateFromVernacular v δv e = do
  trace $ printf "withStateFromVernacular (%s, %s)" (preview @'Chick v) (preview @'Chick δv)

  let exc :: Member (Error String) r => String -> Sem r a
      exc reason = throw @String $ printf "Repair.Script/withStateFromVernacular: %s" reason

  case (v, δv) of
    (Definition defn, _) ->
      withGlobalDef
        ( Binder (Just (D.definitionName defn)),
          D.definitionType defn,
          D.definitionTerm defn
        )
        >>> withδGlobalEnv (vernacularDiffToGlobalEnvironmentDiff δv)
        $ do
          sanityCheck
          e
    ( Inductive ind@(I.Inductive indName ips iis _u cs),
      ΔV.ModifyInductive δind@(ΔI.Modify δindName δips δiis _δu δcs)
      ) -> do
        -- let τind = I.inductiveRawType ind
        let δτind = ΔI.δinductiveRawType (length ips) δips (length iis) δiis
        let prefix = ΔI.δinductiveRawConstructorPrefix δindName (length ips) δips
        δeliminatorType <- case δmkEliminatorType () ind δind of
          Nothing -> throw "δmkEliminatorType failed"
          Just e -> return e
        trace (printf "PREFIX: %s" (show prefix))
        withGlobalInd ind δind $ do
          (env, _) <- getEnvironments
          trace $ printf "Environment after ind:\n%s" (prettyStrU @'Chick env)
          withGlobalAssumIndType ind (δindName, δτind) $
            -- DO NOT MERGE WITH PREVIOUS withGlobalAssumIndType
            -- because mkEliminatorName needs the previous things in scope
            withGlobalAssumAndδ
              (Binder (Just (mkEliminatorName indName)), mkEliminatorRawType ind)
              (δeliminatorName δind, δeliminatorType)
              $ do
                sanityCheck
                withStateFromConstructors prefix ips δips cs δcs e
    (Inductive ind, ΔV.ModifyInductive ΔI.Same) -> unchangedInductive ind
    (Inductive ind, ΔV.Same) -> unchangedInductive ind
    _ -> exc $ printf "TODO: %s" (show (v, δv))
  where
    unchangedInductive ind@(I.Inductive indName ips _ _ cs) =
      withGlobalInd ind ΔI.Same $
        withGlobalAssumIndType ind (ΔA.Same, ΔT.Same) $ do
          trace "UNCHANGED"
          sanityCheck
          withGlobalAssumAndδ
            (Binder (Just (mkEliminatorName indName)), mkEliminatorRawType ind)
            (ΔA.Same, ΔT.Same)
            $ do
              sanityCheck
              withStateFromConstructors ΔT.Same ips ΔL.Same cs ΔL.Same e

-- | `repair s δs` takes a script `s` and a script diff `δs`, and it computes a repaired script diff
-- | `δs'`, that propagates changes down the line
repair ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  Script Raw.Raw Variable ->
  ΔS.Diff Raw.Raw ->
  Sem r (ΔS.Diff Raw.Raw)
repair script@(Script s) δs =
  trace (printf "Repair.Script/repair(s: %s, δs: %s)" (prettyStrU @'Chick script) (prettyStr @'Chick δs))
    >>
    --sanityCheck >>
    traceState
    >> let exc :: Member (Error String) r => String -> Sem r a
           exc reason = throw @String $ printf "Repair.Script/repair: %s" reason
        in case (s, δs) of
             (_, ΔL.Replace s') -> repair (Script s') ΔL.Same
             -- keeping, but still repairing
             (v : s', ΔL.Keep δs') -> do
               δv' <- RV.repair v ΔV.Same
               -- trace $ printf "Repair.Script/repair > δv' %s" (prettyStr δv')
               -- v' <- ΔV.patch v δv'
               -- trace $ printf "VERNAC BEFORE REPAIR: %s" (prettyStr v)
               -- trace $ printf "VERNAC  AFTER REPAIR: %s" (prettyStr v')
               withStateFromVernacular v δv' $
                 ΔL.Modify δv' <$> repair (Script s') δs'
             (v : s', ΔL.Modify δv δs') -> do
               δv' <- RV.repair v δv
               -- trace $ printf "Repair.Script/repair > δv' %s" (prettyStr δv')
               -- v' <- ΔV.patch v δv'
               -- trace $ printf "VERNAC BEFORE REPAIR: %s" (prettyStr v)
               -- trace $ printf "VERNAC  AFTER REPAIR: %s" (prettyStr v')
               withStateFromVernacular v δv' $
                 ΔL.Modify δv' <$> repair (Script s') δs'

             -- even if the diff is same, we still might need to do some repair to account for changes in the
             -- global environment
             ([], ΔL.Same) -> return ΔL.Same
             (v : s', ΔL.Same) -> do
               -- not sure why I was not calling RV.repair here, let's comment this
               -- and try the reasonable thing instead

               -- Note: this can be simplified to a repair (Mod Same Same)
               δv <- RV.repair v ΔV.Same
               withStateFromVernacular v δv $
                 ΔL.Modify δv <$> repair (Script s') ΔL.Same

             --   Inductive _ind -> do
             --     -- I guess this one is weird:
             --     -- - it might be that the inductive type mentions a type that has been updated
             --     -- - it might be that constructor types mention a type that has been updated
             --     return ΔL.Same

             (_, ΔL.Insert _v _δs') -> exc $ printf "TODO/Insert: %s" (show (s, δs))
             ([], ΔL.Modify _δv _δs') -> exc $ printf "TODO/Modify: %s" (show (s, δs))
             (_, ΔL.Permute _p _δs') -> exc $ printf "TODO/Permute: %s" (show (s, δs))
             (_, ΔL.Remove _δs') -> exc $ printf "TODO/Remove: %s" (show (s, δs))
             ([], ΔL.Keep _) -> exc $ printf "Keep on empty program"

runRepair' ::
  Member Trace r =>
  Script Raw.Raw Variable ->
  ΔS.Diff Raw.Raw ->
  Sem r (Either String (Script Raw.Raw Variable))
runRepair' s δs =
  runError
    . fmap snd
    . runState initialRepairState
    $ do
      δs' <- repair s δs
      trace $ printf "COMPUTED PATCH:\n\n%s\n\n" (show δs')
      ΔS.patch s δs'
