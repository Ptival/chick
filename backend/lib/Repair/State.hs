{-# LANGUAGE TemplateHaskell #-}

module Repair.State
  ( RepairState,
    δcontext,
    δenvironment,
    boundVarsInContext,
    context,
    environment,
    getContexts,
    getEnvironments,
    initialRepairState,
    sanityCheck,
    traceState,
    withδGlobalAssum,
    withδGlobalDef,
    withδGlobalEnv,
    withδLocalContext,
    withConstructor,
    withFixpointAndδ,
    withGlobalAssum,
    withGlobalAssumAndδ,
    withGlobalAssumIndType,
    withGlobalDef,
    withGlobalDefAndδ,
    withGlobalInd,
    withInsertLocalAssum,
    withLocalAssum,
    withLocalAssumAndδ,
  )
where

import Control.Arrow ((>>>))
import Control.Lens (makeLenses, over, view)
import qualified Definition as D
import qualified DefinitionObjectKind as DOK
import qualified Diff.Atom as ΔA
import qualified Diff.GlobalDeclaration as ΔGD
import qualified Diff.GlobalEnvironment as ΔGE
import qualified Diff.Inductive as ΔI
import qualified Diff.List as ΔL
import qualified Diff.LocalContext as ΔLC
import qualified Diff.LocalDeclaration as ΔLD
import qualified Diff.Term as ΔT
import Inductive.Inductive (Constructor, Inductive)
import Language (Language (Chick))
import Polysemy (Member, Sem)
import Polysemy.Error (Error)
import Polysemy.State (State, get)
import Polysemy.Trace (Trace, trace)
import PrettyPrinting.Chick ()
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyStr, preview),
  )
import Term.Binder (Binder (Binder))
import qualified Term.Raw as Raw
import Term.Variable (Variable)
import Text.Printf (printf)
import Typing.GlobalEnvironment
  ( GlobalEnvironment (GlobalEnvironment),
    addConstructor,
    addGlobalAssum,
    addGlobalDef,
    addGlobalInd,
    addInductiveType,
  )
import Typing.LocalContext
  ( LocalContext (LocalContext),
    addLocalAssum,
    boundNames,
  )
import Typing.LocalDeclaration (LocalDeclaration (LocalAssum))
import Utils (withState)

data RepairState = RepairState
  { _context :: LocalContext Raw.Raw Variable,
    _δcontext :: ΔLC.Diff Raw.Raw,
    _environment :: GlobalEnvironment Raw.Raw Variable,
    _δenvironment :: ΔGE.Diff Raw.Raw
  }

makeLenses ''RepairState

initialRepairState :: RepairState
initialRepairState =
  RepairState
    (LocalContext [])
    ΔL.Same
    (GlobalEnvironment [])
    ΔL.Same

getContexts ::
  Member (State RepairState) r =>
  Sem r (LocalContext Raw.Raw Variable, ΔLC.Diff Raw.Raw)
getContexts = do
  s <- get
  return (view context s, view δcontext s)

getEnvironments ::
  Member (State RepairState) r =>
  Sem r (GlobalEnvironment Raw.Raw Variable, ΔGE.Diff Raw.Raw)
getEnvironments = do
  s <- get
  return (view environment s, view δenvironment s)

traceState ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  Sem r ()
traceState = do
  RepairState γ δγ e δe <- get
  trace $ printf "RepairState:"
  trace $ printf "> γ: %s" (prettyStr @'Chick γ)
  --trace $ printf "> δγ: %s" (prettyStr δγ)
  γ' <- ΔLC.patch γ δγ
  trace $ printf "> γ': %s" (prettyStr @'Chick γ')
  trace $ printf "> e: %s" (prettyStr @'Chick e)
  --trace $ printf "> δe: %s" (prettyStr δe)
  e' <- ΔGE.patch e δe
  trace $ printf "> e': %s" (prettyStr @'Chick e')

boundVarsInContext ::
  Member (State RepairState) r =>
  Sem r [Variable]
boundVarsInContext = boundNames . view context <$> get

sanityCheck ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  Sem r ()
sanityCheck = do
  trace "*** SANITY CHECK ***"
  RepairState γ δγ e δe <- get
  trace "Sanity-checking local context"
  _ <- ΔLC.patch γ δγ
  trace "Sanity-checking global environment"
  _ <- ΔGE.patch e δe
  return ()

withGlobalAssum ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  (Binder Variable, Raw.Term Variable) ->
  Sem r a ->
  Sem r a
withGlobalAssum (b, τ) e = do
  trace $ printf "Global Assumption (%s : %s)" (preview @'Chick b) (preview @'Chick τ)
  withState (over environment (addGlobalAssum (b, τ))) e

withδGlobalAssum ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  (ΔA.Diff Variable, ΔT.Diff Raw.Raw) ->
  Sem r a ->
  Sem r a
withδGlobalAssum (δb, δτ) e = do
  trace $ printf "Modifying Global Assumption (%s : %s)" (preview @'Chick δb) (preview @'Chick δτ)
  withState (over δenvironment (ΔL.Modify (ΔGD.ModifyGlobalAssum δb δτ))) e

withGlobalAssumAndδ ::
  ( Member (Error String) r,
    Member Trace r,
    Member (State RepairState) r
  ) =>
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  (Binder Variable, Raw.Term Variable) ->
  (ΔA.Diff Variable, ΔT.Diff Raw.Raw) ->
  Sem r a ->
  Sem r a
withGlobalAssumAndδ (b, τ) (δb, δτ) e =
  withGlobalAssum (b, τ) >>> withδGlobalAssum (δb, δτ) $ do
    sanityCheck
    e

withGlobalDef ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  (Binder Variable, Raw.Type Variable, Raw.Term Variable) ->
  Sem r a ->
  Sem r a
withGlobalDef (b, τ, t) e = do
  trace $ printf "Global Definition (%s : %s := %s)" (preview @'Chick b) (preview @'Chick τ) (preview @'Chick t)
  withState (over environment (addGlobalDef (b, τ, t))) e

withδGlobalDef ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  (ΔA.Diff Variable, ΔT.Diff Raw.Raw, ΔT.Diff Raw.Raw) ->
  Sem r a ->
  Sem r a
withδGlobalDef (δb, δτ, δt) e = do
  trace $ printf "Modifying Global Definition (%s : %s := %s)" (preview @'Chick δb) (preview @'Chick δτ) (preview @'Chick δt)
  withState (over δenvironment (ΔL.Modify (ΔGD.ModifyGlobalDef δb δτ δt))) e

withGlobalDefAndδ ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  (Binder Variable, Raw.Term Variable, Raw.Term Variable) ->
  (ΔA.Diff Variable, ΔT.Diff Raw.Raw, ΔT.Diff Raw.Raw) ->
  Sem r a ->
  Sem r a
withGlobalDefAndδ d δd e =
  withGlobalDef d >>> withδGlobalDef δd $ do
    sanityCheck
    e

withδLocalContext ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  (ΔLC.Diff Raw.Raw -> ΔLC.Diff Raw.Raw) ->
  Sem r a ->
  Sem r a
withδLocalContext δ = withState (over δcontext δ)

withδGlobalEnv ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  (ΔGE.Diff Raw.Raw -> ΔGE.Diff Raw.Raw) ->
  Sem r a ->
  Sem r a
withδGlobalEnv δ = withState (over δenvironment δ)

withLocalAssum ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  (Binder Variable, Raw.Term Variable) ->
  Sem r a ->
  Sem r a
withLocalAssum (b, τ) e = do
  trace $ printf "Local Assumption (%s : %s)" (preview @'Chick b) (preview @'Chick τ)
  withState (over context (addLocalAssum (b, τ))) e

withδLocalAssum ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  (ΔA.Diff (Binder Variable), ΔT.Diff Raw.Raw) ->
  Sem r a ->
  Sem r a
withδLocalAssum (δb, δτ) e = do
  trace $ printf "δ Local Assumption (%s : %s)" (preview @'Chick δb) (preview @'Chick δτ)
  withState (over δcontext (ΔL.Modify (ΔLD.ModifyLocalAssum δb δτ))) e

withLocalAssumAndδ ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  (Binder Variable, Raw.Term Variable) ->
  (ΔA.Diff (Binder Variable), ΔT.Diff Raw.Raw) ->
  Sem r a ->
  Sem r a
withLocalAssumAndδ (b, τ) (δb, δτ) e =
  withLocalAssum (b, τ) >>> withδLocalAssum (δb, δτ) $ do
    sanityCheck
    e

withInsertLocalAssum ::
  Member (State RepairState) r =>
  (Binder Variable, Raw.Term Variable) ->
  Sem r a ->
  Sem r a
withInsertLocalAssum (b, τ) =
  withState (over δcontext (ΔL.Insert (LocalAssum b τ)))

withConstructor ::
  Member (State RepairState) r =>
  Constructor Raw.Raw Variable ->
  Sem r a ->
  Sem r a
withConstructor c = withState (over environment (addConstructor c))

withGlobalInd ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  Inductive Raw.Raw Variable ->
  ΔI.Diff Raw.Raw ->
  Sem r a ->
  Sem r a
withGlobalInd ind δind e = do
  trace "TODO"
  ( withState (over environment (addGlobalInd ind))
      >>> withState (over δenvironment (ΔL.Modify (ΔGD.ModifyGlobalInd δind)))
    )
    $ do
      sanityCheck
      e

withGlobalAssumIndType ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  Inductive Raw.Raw Variable ->
  (ΔA.Diff Variable, ΔT.Diff Raw.Raw) ->
  Sem r a ->
  Sem r a
withGlobalAssumIndType ind (δb, δτ) e = do
  trace "TODO"
  ( withState (over environment (addInductiveType ind))
      >>> withState (over δenvironment (ΔL.Modify (ΔGD.ModifyGlobalAssum δb δτ)))
    )
    $ do
      sanityCheck
      e

withFixpointAndδ ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  D.Definition Raw.Raw Variable ->
  (ΔA.Diff Variable, ΔT.Diff Raw.Raw, ΔT.Diff Raw.Raw) ->
  Sem r a ->
  Sem r a
withFixpointAndδ d (δb, δτ, δt) e =
  case D.definitionKind d of
    DOK.Fixpoint ->
      -- here we don't have δt yet since we are computing it...
      -- TODO: check that putting ΔT.Same does not cause problems
      withGlobalDefAndδ
        (Binder (Just (D.definitionName d)), D.definitionType d, D.definitionTerm d)
        (δb, δτ, δt)
        e
    DOK.Definition ->
      e
