{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

module Repair.Inductive
  ( repair
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace

import qualified Diff.Inductive as DI
import qualified Inductive.Inductive as I
import           Repair.State
import qualified Term.Raw as Raw
import           Term.Term

repair ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  I.Inductive Raw.Raw Variable -> DI.Diff Raw.Raw -> Eff r (DI.Diff Raw.Raw)
repair (I.Inductive _n _ps _is _u _cs) = \case

  DI.Modify δn δps δis δu δcs -> do
    -- FIXME
    return $ DI.Modify δn δps δis δu δcs

  DI.Same -> do
    -- FIXME
    return $ DI.Same
