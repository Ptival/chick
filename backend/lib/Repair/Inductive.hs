{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

module Repair.Inductive
  ( repair
  ) where

import           Polysemy       ( Member, Sem )
import           Polysemy.Error ( Error )
import           Polysemy.State ( State )
import           Polysemy.Trace ( Trace )

import qualified Diff.Inductive as DI
import qualified Inductive.Inductive as I
import           Repair.State
import qualified Term.Raw as Raw
import           Term.Term

repair ::
  Member (Error String)      r =>
  Member Trace               r =>
  Member (State RepairState) r =>
  I.Inductive Raw.Raw Variable -> DI.Diff Raw.Raw -> Sem r (DI.Diff Raw.Raw)
repair (I.Inductive _n _ps _is _u _cs) = \case

  DI.Modify δn δps δis δu δcs -> do
    -- FIXME
    return $ DI.Modify δn δps δis δu δcs

  DI.Same -> do
    -- FIXME
    return $ DI.Same
