{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Repair.State
  ( RepairState (..)
  , boundVarsInContext
  -- , context
  -- , δcontext
  -- , environment
  -- , δenvironment
  , sanityCheck
  , traceState
  , withδGlobalAssum
  , withδGlobalDef
  , withδGlobalEnv
  , withδLocalContext
  , withConstructor
  , withGlobalAssum
  , withGlobalAssumAndδ
  , withGlobalAssumIndType
  , withGlobalDef
  , withGlobalDefAndδ
  , withGlobalInd
  , withInsertLocalAssum
  , withLocalAssum
  , withLocalAssumAndδ
  ) where

import           Control.Arrow ((>>>))
import           Control.Lens (makeLenses, over, view)
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Text.Printf (printf)

import qualified Diff.Atom as ΔA
import qualified Diff.GlobalDeclaration as ΔGD
import qualified Diff.GlobalEnvironment as ΔGE
import qualified Diff.Inductive as ΔI
import qualified Diff.List as ΔL
import qualified Diff.LocalDeclaration as ΔLD
import qualified Diff.LocalContext as ΔLC
import qualified Diff.Term as ΔT
import           Inductive.Inductive
import           PrettyPrinting.PrettyPrintable
import           Term.Term
import qualified Term.Raw as Raw
import           Typing.GlobalEnvironment
import           Typing.LocalContext
import           Typing.LocalDeclaration
import           Utils

data RepairState = RepairState
  { _context      :: LocalContext      Raw.Raw Variable
  , _δcontext     :: ΔLC.Diff          Raw.Raw
  , _environment  :: GlobalEnvironment Raw.Raw Variable
  , _δenvironment :: ΔGE.Diff          Raw.Raw
  }

makeLenses ''RepairState

traceState ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  Eff r ()
traceState = do
  RepairState γ δγ e δe <- get
  trace $ printf "RepairState:"
  trace $ printf "> γ: %s" (prettyStr γ)
  --trace $ printf "> δγ: %s" (prettyStr δγ)
  γ' <- ΔLC.patch γ δγ
  trace $ printf "> γ': %s" (prettyStr γ')
  trace $ printf "> e: %s" (prettyStr e)
  --trace $ printf "> δe: %s" (prettyStr δe)
  e' <- ΔGE.patch e δe
  trace $ printf "> e': %s" (prettyStr e')

boundVarsInContext ::
  ( Member (State RepairState) r
  ) =>
  Eff r [Variable]
boundVarsInContext = boundNames . view context <$> get

sanityCheck ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  Eff r ()
sanityCheck = do
  trace "*** SANITY CHECK ***"
  RepairState γ δγ e δe <- get
  trace "Sanity-checking local context"
  _ <- ΔLC.patch γ δγ
  trace "Sanity-checking global environment"
  _ <- ΔGE.patch e δe
  return ()

withGlobalAssum ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  (Binder Variable,  Raw.Term Variable) ->
  Eff r a -> Eff r a
withGlobalAssum (b, τ) e = do
  trace $ printf "Global Assumption (%s : %s)" (preview b) (preview τ)
  withState (over environment (addGlobalAssum (b, τ))) e

withδGlobalAssum ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  (ΔA.Diff Variable, ΔT.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withδGlobalAssum (δb, δτ) e = do
  trace $ printf "Modifying Global Assumption (%s : %s)" (preview δb) (preview δτ)
  withState (over δenvironment (ΔL.Modify (ΔGD.ModifyGlobalAssum δb δτ))) e

withGlobalAssumAndδ ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  (Binder Variable,  Raw.Term Variable) ->
  (ΔA.Diff Variable, ΔT.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withGlobalAssumAndδ (b, τ) (δb, δτ) e = do
  withGlobalAssum (b, τ) >>> withδGlobalAssum (δb, δτ) $ do
    sanityCheck
    e

withGlobalDef ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  (Binder Variable,  Raw.Term Variable, Raw.Term Variable) ->
  Eff r a -> Eff r a
withGlobalDef (b, τ, t) e = do
  trace $ printf "Global Definition (%s : %s := %s)" (preview b) (preview τ) (preview t)
  withState (over environment (addGlobalDef (b, τ, t))) e

withδGlobalDef ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  (ΔA.Diff Variable, ΔT.Diff Raw.Raw,   ΔT.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withδGlobalDef (δb, δτ, δt) e = do
  trace $ printf "Modifying Global Definition (%s : %s := %s)" (preview δb) (preview δτ) (preview δt)
  withState (over δenvironment (ΔL.Modify (ΔGD.ModifyGlobalDef δb δτ δt))) e

withGlobalDefAndδ ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  (Binder Variable,  Raw.Term Variable, Raw.Term Variable) ->
  (ΔA.Diff Variable, ΔT.Diff Raw.Raw,   ΔT.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withGlobalDefAndδ d δd e = do
  withGlobalDef d >>> withδGlobalDef δd $ do
    sanityCheck
    e

withδLocalContext ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  (ΔLC.Diff Raw.Raw -> ΔLC.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withδLocalContext δ = withState (over δcontext δ)

withδGlobalEnv ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  (ΔGE.Diff Raw.Raw -> ΔGE.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withδGlobalEnv δ = withState (over δenvironment δ)

withLocalAssum ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  (Binder Variable,  Raw.Term Variable) ->
  Eff r a -> Eff r a
withLocalAssum (b, τ) e = do
  trace $ printf "Local Assumption (%s : %s)" (preview b) (preview τ)
  withState (over context (addLocalAssum (b, τ))) e

withδLocalAssum ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  (ΔA.Diff (Binder Variable), ΔT.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withδLocalAssum (δb, δτ) e = do
  trace $ printf "δ Local Assumption (%s : %s)" (preview δb) (preview δτ)
  withState (over δcontext (ΔL.Modify (ΔLD.ModifyLocalAssum δb δτ))) e

withLocalAssumAndδ ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  (Binder Variable,  Raw.Term Variable) ->
  (ΔA.Diff (Binder Variable), ΔT.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withLocalAssumAndδ (b, τ) (δb, δτ) e = do
  withLocalAssum (b, τ) >>> withδLocalAssum (δb, δτ) $ do
    sanityCheck
    e

withInsertLocalAssum ::
  ( Member (State RepairState) r
  ) =>
  (Binder Variable, Raw.Term Variable) ->
  Eff r a -> Eff r a
withInsertLocalAssum (b, τ) =
  withState (over δcontext (ΔL.Insert (LocalAssum b τ)))

withConstructor ::
  ( Member (State RepairState) r
  ) =>
  Constructor Raw.Raw Variable ->
  Eff r a -> Eff r a
withConstructor c = withState (over environment (addConstructor c))

withGlobalInd ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  Inductive Raw.Raw Variable -> ΔI.Diff Raw.Raw ->
  Eff r a -> Eff r a
withGlobalInd ind δind e = do
  trace "TODO"
  (     withState (over environment (addGlobalInd ind))
    >>> withState (over δenvironment (ΔL.Modify (ΔGD.ModifyGlobalInd δind)))
    ) $ do
    sanityCheck
    e

withGlobalAssumIndType ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  Inductive Raw.Raw Variable ->
  (ΔA.Diff Variable, ΔT.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withGlobalAssumIndType ind (δb, δτ) e = do
  trace "TODO"
  (     withState (over environment (addInductiveType ind))
    >>> withState (over δenvironment (ΔL.Modify (ΔGD.ModifyGlobalAssum δb δτ)))
    ) $ do
    sanityCheck
    e
