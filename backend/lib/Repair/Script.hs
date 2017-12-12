{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Repair.Script
  ( repair
  ) where

import           Control.Arrow
-- import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Text.Printf

import qualified Diff.Atom as ΔA
import qualified Diff.Constructor as ΔC
import           Diff.Eliminator
import qualified Diff.GlobalDeclaration as ΔGD
import qualified Diff.GlobalEnvironment as ΔGE
import qualified Diff.Inductive as ΔI
import qualified Diff.List as ΔL
import qualified Diff.Script as ΔS
import qualified Diff.Term as ΔT
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
-- import qualified Typing.LocalContext as LC
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
withStateFromConstructors prefix ips δips l δl e =

  trace "ADΔING A CONSTRUCTOR" >>
  traceState >>

  let todo (s :: String) = error $ printf "TODO: Repair.Script/wSFC %s" s in
  case (l, δl) of

    ([], ΔL.Same) -> e

    ([], _) -> todo "Empty list, other cases"

    (_c : _cs, ΔL.Insert  _  _)   -> todo "Insert"
    (_c : _cs, ΔL.Keep    _)      -> todo "Keep"

    ( c@(I.Constructor _ _consName cps cis) : cs,
      ΔL.Modify (ΔC.Modify δconsName δcps δcis) δcs) ->
      let δτc = ΔI.δconstructorRawType prefix ips δips cps δcps cis δcis in
      go c (ΔL.Modify (ΔGD.ModifyGlobalAssum δconsName δτc)) cs δcs

    (_c : _cs, ΔL.Modify _ _)     -> todo "Modify"
    (_c : _cs, ΔL.Permute _  _)   -> todo "Permute"
    (_c : _cs, ΔL.Remove  _)      -> todo "Remove"
    (_c : _cs, ΔL.Replace _)      -> todo "Replace"
    (c : cs, ΔL.Same) -> go c ΔL.Keep cs ΔL.Same

    where
    go c δge cs δcs =
      withConstructor c >>> withδGlobalEnv δge $ do
      sanityCheck
      withStateFromConstructors prefix ips δips cs δcs $ do
        e

-- | `withStateFromVernacular v δv` takes a vernacular command `v` and its (assumed repaired) diff `δv`
-- | and modifies the global environment to accound for the effect in the vernacular command before and
-- | after changes.
withStateFromVernacular ::
  ( Member (Exc String) r
  , Member (State RepairState) r
  , Member Trace r
  ) => Vernacular Raw.Raw Variable -> ΔV.Diff Raw.Raw -> Eff r a -> Eff r a
withStateFromVernacular v δv e =

  let exc (reason :: String) =
        throwExc $ printf "Repair.Script/withStateFromVernacular: %s" reason
  in

  case (v, δv) of

  (Definition _ n τ t, _) ->
    withGlobalDef (Binder (Just n), τ, t)
    >>> withδGlobalEnv (vernacularDiffToGlobalEnvironmentDiff δv)
    $ do
    sanityCheck
    e

  (Inductive ind@(I.Inductive indName ips iis _u cs)
    , ΔV.ModifyInductive δind@(ΔI.Modify δindName δips δiis _δu δcs)) -> do
    let τind = I.inductiveRawType ind
    let δτind = ΔI.δinductiveRawType (length ips) δips (length iis) δiis
    let prefix = ΔI.δinductiveRawConstructorPrefix δindName (length ips) δips
    δeliminatorType <- case δmkEliminatorType () ind δind of
      Nothing -> throwError "δmkEliminatorType failed"
      Just e -> return e
    trace (printf "PREFIX: %s" (show prefix))
    withInductive ind
      >>> withModifyGlobalInd δind
      >>> withInductiveType ind
      >>> withModifyGlobalAssum (δindName, δτind)
      $ do
      -- DO NOT MERGE WITH PREVIOUS withState
      -- because mkEliminatorName needs the previous things in scope
      withGlobalAssum ( Binder (Just (mkEliminatorName indName))
                      , mkEliminatorRawType ind)
        >>> withModifyGlobalAssum (δeliminatorName δind, δeliminatorType)
        $ do
        sanityCheck
        withStateFromConstructors prefix ips δips cs δcs e

  (Inductive ind, ΔV.ModifyInductive ΔI.Same) -> unchangedInductive ind
  (Inductive ind, ΔV.Same)                    -> unchangedInductive ind

  _ -> exc $ printf "TODO: %s" (show (v, δv))

  where
    unchangedInductive ind@(I.Inductive indName ips _ _ cs) = do
      let τind = I.inductiveRawType ind
      withGlobalAssum (Binder . Just $ indName, τind)
        >>> withδGlobalEnv ΔL.Keep
        >>> withGlobalAssum ( Binder (Just (mkEliminatorName indName))
                        , mkEliminatorRawType ind)
        >>> withδGlobalEnv  ΔL.Keep
        $ do
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

    (cmd : cmds, ΔL.Same) ->
      case cmd of

        -- eventually, might want to update the name in case of collision?
        def@(Definition b n τ t) -> do
          trace $ printf "*** Attempting to repair %s" (prettyStr def)
          δτ <- RT.repair τ (Type U.Type) ΔT.Same
          trace $ printf "*** Repaired type, δτ: %s" (prettyStr δτ)
          τ' <- ΔT.patch τ δτ
          trace $ printf "*** Repaired type, τ': %s" (prettyStr τ')
          let scopeIfFixpoint =
                if b
                then withGlobalDef ((Binder (Just n)),  τ,  t)
                     -- here we don't have δt yet since we are computing it...
                     -- TODO: check that putting ΔT.Same does not cause problems
                     >>> withModifyGlobalDef (ΔA.Same, δτ, ΔT.Same)
                else id
          δt <- scopeIfFixpoint $ RT.repair t τ δτ
          trace $ printf "*** Repaired term, δt: %s" (prettyStr δt)
          t' <- ΔT.patch t δt
          trace $ printf "*** Repaired term, t': %s" (prettyStr t')
          withGlobalDef ((Binder (Just n)),  τ,  t)
            >>> withModifyGlobalDef (ΔA.Same, δτ, δt)
            $ do
            trace "Repair.Script:180"
            sanityCheck
            ΔL.Modify (ΔV.ModifyDefinition ΔA.Same ΔA.Same δτ δt)
              <$> repair (Script cmds) ΔL.Same

        Inductive _ind -> do
          -- I guess this one is weird:
          -- - it might be that the inductive type mentions a type that has been updated
          -- - it might be that constructor types mention a type that has been updated
          return ΔL.Same

    --_ -> exc $ printf "TODO: %s" (show (s, δs))
