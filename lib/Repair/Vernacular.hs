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

-- import qualified Diff.Atom as DA
-- import qualified Diff.GlobalDeclaration as DGD
-- import qualified Diff.GlobalEnvironment as DGE
import qualified Diff.Inductive as DI
-- import qualified Diff.List as DL
-- import qualified Diff.Script as DS
import qualified Diff.Term as DT
import           Diff.Utils
import qualified Diff.Vernacular as DV
import qualified Inductive.Inductive as I
import           PrettyPrinting.PrettyPrintableUnannotated
import           Repair.State
import qualified Repair.Term as RT
-- import           Script
-- import           Term.Binder
import qualified Term.Raw as Raw
-- import           Term.Term
import           Term.Variable
-- import qualified Typing.GlobalEnvironment as GE
-- import qualified Typing.LocalContext as LC
-- import           Utils
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
  trace (printf "Repair.Vernacular/repair:\nv: %s\nδv: %s\n" (prettyStrU v) (show δv)) >>
  let exc (reason :: String) = throwExc $ printf "Repair.Vernacular/repair: %s" reason in
  case (v, δv) of

    (Definition _ τ t, DV.ModifyDefinition δn δτ DT.Same) -> do
      δt <- RT.repair t τ δτ
      trace $ printf "Repair term: %s" (show δt)
      return $ DV.ModifyDefinition δn δτ δt

    (Definition _ _ _, DV.ModifyDefinition _ _ _) -> do
      exc $ "TODO: 1"

    (_, DV.ModifyDefinition _ _ _) -> exc "ModifyDefinition, but not a Definition"

    (Inductive (I.Inductive _ _ps _is _), DV.ModifyInductive (DI.Modify δn δps δis δcs)) -> do
      -- TODO: actually propagate repairs into params and constructors
      return $ DV.ModifyInductive (DI.Modify δn δps δis δcs)

    _ -> exc $ printf "TODO: %s" (show δv)
