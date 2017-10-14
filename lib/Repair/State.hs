{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Repair.State
  ( RepairState (..)
  , context
  , δcontext
  , environment
  , δenvironment
  , sanityCheck
  , traceState
  , withScopedGlobalDef
  ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Text.Printf

import qualified Diff.Atom as ΔA
import qualified Diff.GlobalDeclaration as ΔGD
import qualified Diff.GlobalEnvironment as ΔGE
import qualified Diff.List as ΔL
import qualified Diff.LocalContext as ΔLC
import qualified Diff.Term as ΔT
import           PrettyPrinting.PrettyPrintable
import           Term.Term
import qualified Term.Raw as Raw
import           Typing.GlobalEnvironment
import           Typing.LocalContext
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

withScopedGlobalDef ::
  ( Member (State RepairState) r
  ) =>
  (Binder Variable,  Raw.Term Variable, Raw.Term Variable) ->
  (ΔA.Diff Variable, ΔT.Diff Raw.Raw,   ΔT.Diff Raw.Raw) ->
  Eff r a -> Eff r a
withScopedGlobalDef (b, τ, t) (δb, δτ, δt) e =
  withState
  (   over  environment (addGlobalDef (b, τ, t))
  >>> over δenvironment (ΔL.Modify (ΔGD.ModifyGlobalDef δb δτ δt)))
  $ e
