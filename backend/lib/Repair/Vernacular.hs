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

import qualified Definition as D
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
  trace (printf "Repair.Vernacular/repair:\nv: %s\nδv: %s\n"
         (prettyStrU v) (prettyStr δv)) >>
  let exc (reason :: String) =
        throwExc $ printf "Repair.Vernacular/repair: %s" reason in
  case (v, δv) of

    (Definition d, ΔV.ModifyDefinition δb δn δτ δt) -> do
      -- WARNING: here I assume that the modification did not start refactoring
      -- otherwise, we might get in double-repair issues
      trace $ printf "Definition: %s" (show d)
      trace $ printf "ModifyDefinition: %s" (show (δb, δn, δτ, δt))
      exc "This happens"
      -- FIXME: not sure what I want to do here...
      -- δτ <- RT.repair (D.definitionType d)
      -- t' <- ΔT.patch (D.definitionTerm d) δt
      -- δt' <- RT.repair t' (D.definitionType d) δτ
      return $ ΔV.ModifyDefinition δb δn δτ δt

    (_, ΔV.ModifyDefinition _ _ _ _) ->
      exc "ModifyDefinition, but not a Definition"

    (Inductive ind, ΔV.ModifyInductive δind) -> do
      ΔV.ModifyInductive <$> RI.repair ind δind

    (Definition d, ΔV.Same) -> do
      -- FIXME: need to deal with b
      δτ <- RT.repair (D.definitionType d) (Type U.Type) ΔT.Same
      -- here we have to put ΔT.Same because we don't know the body repair yet,
      -- but this should only be temporary/local to repairing the body itself
      δt <- withFixpointAndδ d (ΔA.Same, δτ, ΔT.Same) $ do
        RT.repair (D.definitionTerm d) (D.definitionType d) δτ
      return $ ΔV.ModifyDefinition ΔA.Same ΔA.Same δτ δt

    (Inductive ind, ΔV.Same) -> do
      δind <- RI.repair ind ΔI.Same
      return $ ΔV.ModifyInductive δind

    _ -> exc $ printf "TODO: %s" (show δv)
