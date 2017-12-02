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
  , withδGlobalEnv
  , withδLocalContext
  , withConstructor
  , withGlobalAssum
  , withGlobalDef
  , withInductive
  , withInductiveType
  , withInsertLocalAssum
  , withLocalAssum
  , withModifyGlobalAssum
  , withModifyGlobalDef
  , withModifyGlobalInd
  , withModifyLocalAssum
  ) where

import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Text.Printf

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
  _ <- ΔLC.patch γ δγ
  _ <- ΔGE.patch e δe
  return ()

withGlobalAssum ::
  ( Member (State RepairState) r
  ) =>
  (Binder Variable,  Raw.Term Variable) ->
  Eff r a -> Eff r a
withGlobalAssum (b, τ) =
  withState (over environment (addGlobalAssum (b, τ)))

withGlobalDef ::
  ( Member (State RepairState) r
  ) =>
  (Binder Variable,  Raw.Term Variable, Raw.Term Variable) ->
  Eff r a -> Eff r a
withGlobalDef (b, τ, t) =
  withState (over environment (addGlobalDef (b, τ, t)))

withModifyGlobalAssum ::
  ( Member (State RepairState) r
  ) =>
  (ΔA.Diff Variable, ΔT.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withModifyGlobalAssum (δb, δτ) =
  withState (over δenvironment (ΔL.Modify (ΔGD.ModifyGlobalAssum δb δτ)))

withModifyGlobalDef ::
  ( Member (State RepairState) r
  ) =>
  (ΔA.Diff Variable, ΔT.Diff Raw.Raw,   ΔT.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withModifyGlobalDef (δb, δτ, δt) =
  withState (over δenvironment (ΔL.Modify (ΔGD.ModifyGlobalDef δb δτ δt)))

withLocalAssum ::
  ( Member (State RepairState) r
  ) =>
  (Binder Variable,  Raw.Term Variable) ->
  Eff r a -> Eff r a
withLocalAssum (b, τ) = withState (over context (addLocalAssum (b, τ)))

withδLocalContext ::
  ( Member (State RepairState) r
  ) =>
  (ΔLC.Diff Raw.Raw -> ΔLC.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withδLocalContext δ = withState (over δcontext δ)

withδGlobalEnv ::
  ( Member (State RepairState) r
  ) =>
  (ΔGE.Diff Raw.Raw -> ΔGE.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withδGlobalEnv δ = withState (over δenvironment δ)

withModifyLocalAssum ::
  ( Member (State RepairState) r
  ) =>
  (ΔA.Diff (Binder Variable), ΔT.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withModifyLocalAssum (δb, δτ) =
  withState (over δcontext (ΔL.Modify (ΔLD.ModifyLocalAssum δb δτ)))

withModifyGlobalInd ::
  ( Member (State RepairState) r
  ) =>
  ΔI.Diff Raw.Raw ->
  Eff r a -> Eff r a
withModifyGlobalInd δind =
  withState (over δenvironment (ΔL.Modify (ΔGD.ModifyGlobalInd δind)))

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

withInductive ::
  ( Member (State RepairState) r
  ) =>
  Inductive Raw.Raw Variable ->
  Eff r a -> Eff r a
withInductive i = withState (over environment (addGlobalInd i))

withInductiveType ::
  ( Member (State RepairState) r
  ) =>
  Inductive Raw.Raw Variable ->
  Eff r a -> Eff r a
withInductiveType ind =
  withState (over environment (addInductiveType ind))
