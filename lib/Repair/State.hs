{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Repair.State
  ( RepairState (..)
  , context
  , δcontext
  , environment
  , δenvironment
  , traceState
  ) where

import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Text.Printf

import qualified Diff.GlobalEnvironment as DGE
import qualified Diff.LocalContext as DLC
import           PrettyPrinting.PrettyPrintableUnannotated
import qualified Term.Raw as Raw
import           Term.Variable
import           Typing.GlobalEnvironment
import           Typing.LocalContext

data RepairState = RepairState
  { _context      :: LocalContext      Raw.Raw Variable
  , _δcontext     :: DLC.Diff          Raw.Raw
  , _environment  :: GlobalEnvironment Raw.Raw Variable
  , _δenvironment :: DGE.Diff          Raw.Raw
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
  trace $ printf "RepairState:\n> γ: %s\n> δγ: %s" (prettyStrU γ) (show δγ)
  γ' <- DLC.patch γ δγ
  trace $ printf "> γ': %s" (prettyStrU γ')
  trace $ printf "> e: %s\n> δe: %s" (prettyStrU e) (show δe)
  e' <- DGE.patch e δe
  trace $ printf "> e': %s\n" (prettyStrU e')
