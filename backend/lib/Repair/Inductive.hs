module Repair.Inductive
  ( repair,
  )
where

import qualified Diff.Inductive as DI
import qualified Inductive.Inductive as I
import Polysemy (Member, Sem)
import Polysemy.Error (Error)
import Polysemy.State (State)
import Polysemy.Trace (Trace)
import Repair.State (RepairState)
import qualified Term.Raw as Raw
import Term.Variable (Variable)

repair ::
  Member (Error String) r =>
  Member Trace r =>
  Member (State RepairState) r =>
  I.Inductive Raw.Raw Variable ->
  DI.Diff Raw.Raw ->
  Sem r (DI.Diff Raw.Raw)
repair (I.Inductive _n _ps _is _u _cs) = \case
  DI.Modify δn δps δis δu δcs ->
    -- FIXME
    return $ DI.Modify δn δps δis δu δcs
  DI.Same ->
    -- FIXME
    return DI.Same
