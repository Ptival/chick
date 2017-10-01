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

import qualified Diff.Atom as DA
import qualified Diff.Inductive as DI
import qualified Diff.Term as DT
import           Diff.Utils
import qualified Diff.Vernacular as DV
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
  Vernacular Raw.Raw Variable -> DV.Diff Raw.Raw -> Eff r (DV.Diff Raw.Raw)
repair v δv =
  trace (printf "Repair.Vernacular/repair:\nv: %s\nδv: %s\n" (prettyStrU v) (prettyStr δv)) >>
  let exc (reason :: String) = throwExc $ printf "Repair.Vernacular/repair: %s" reason in
  case (v, δv) of

    (Definition _ τ t, DV.ModifyDefinition δn δτ DT.Same) -> do
      δt <- RT.repair t τ δτ
      trace $ printf "Repair term: %s" (show δt)
      return $ DV.ModifyDefinition δn δτ δt

    (Definition _ _ _, DV.ModifyDefinition _ _ _) -> do
      exc $ "TODO: 1"

    (_, DV.ModifyDefinition _ _ _) -> exc "ModifyDefinition, but not a Definition"

    (Inductive ind, DV.ModifyInductive δind) -> do
      DV.ModifyInductive <$> RI.repair ind δind

    (Definition _ τ t, DV.Same) -> do
      δτ <- RT.repair τ (Type U.Type) DT.Same
      δt <- RT.repair t τ δτ
      return $ DV.ModifyDefinition DA.Same δτ δt

    (Inductive ind, DV.Same) -> do
      δind <- RI.repair ind DI.Same
      return $ DV.ModifyInductive δind

    _ -> exc $ printf "TODO: %s" (show δv)
