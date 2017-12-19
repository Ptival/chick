{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Repair.Vernacular
  ( repair
  ) where

-- import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Text.Printf

import qualified Diff.Atom as ΔA
import qualified Diff.Inductive as ΔI
import qualified Diff.Term as ΔT
import           Diff.Utils
import qualified Diff.Vernacular as ΔV
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import qualified Repair.Inductive as RI
import           Repair.State
import qualified Repair.Term as RT
import qualified Term.Raw as Raw
import           Term.Term
import qualified Term.Universe as U
import           Vernacular

-- | `repair v δv` takes a vernacular command `v` and a command diff `δv`, and it computes a repaired
-- | command diff `δv'`, that propagates changes down the line
repair ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  Vernacular Raw.Raw Variable -> ΔV.Diff Raw.Raw -> Eff r (ΔV.Diff Raw.Raw)
repair v δv =
  trace (printf "Repair.Vernacular/repair:\nv: %s\nδv: %s\n" (prettyStrU v) (prettyStr δv)) >>
  let exc (reason :: String) = throwExc $ printf "Repair.Vernacular/repair: %s" reason in
  case (v, δv) of

    (Definition _b _ τ t, ΔV.ModifyDefinition δb δn δτ ΔT.Same) -> do
      -- FIXME: need to deal with b
      δt <- RT.repair t τ δτ
      trace $ printf "Repair term: %s" (show δt)
      return $ ΔV.ModifyDefinition δb δn δτ δt

    -- TODO: can probably merge this with the previous one at some point
    (Definition _ _ τ t, ΔV.ModifyDefinition δb δn δτ δt) -> do
      -- FIXME: here I assume that the modification did not start refactoring
      -- otherwise, we might get in double-repair issues
      t' <- ΔT.patch t δt
      δt' <- RT.repair t' τ δτ
      return $ ΔV.ModifyDefinition δb δn δτ δt'

    (_, ΔV.ModifyDefinition _ _ _ _) ->
      exc "ModifyDefinition, but not a Definition"

    (Inductive ind, ΔV.ModifyInductive δind) -> do
      ΔV.ModifyInductive <$> RI.repair ind δind

    (Definition _b _ τ t, ΔV.Same) -> do
      -- FIXME: need to deal with b
      δτ <- RT.repair τ (Type U.Type) ΔT.Same
      δt <- RT.repair t τ δτ
      return $ ΔV.ModifyDefinition ΔA.Same ΔA.Same δτ δt

    (Inductive ind, ΔV.Same) -> do
      δind <- RI.repair ind ΔI.Same
      return $ ΔV.ModifyInductive δind

    _ -> exc $ printf "TODO: %s" (show δv)
