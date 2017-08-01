{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Repair.Inductive
  ( propagate
  -- , repair
  ) where

import qualified Diff.Atom as DA
import qualified Diff.Constructor as DC
import qualified Diff.GlobalDeclaration as DGD
import qualified Diff.GlobalEnvironment as DGE
import qualified Diff.Inductive as DI
import qualified Diff.List as DL
import qualified Diff.Script as DS
import qualified Diff.Term as DT
import           Diff.Utils
import qualified Diff.Vernacular as DV
import qualified Inductive.Constructor as C
import qualified Inductive.Inductive as I
import           PrettyPrinting.PrettyPrintableUnannotated
import           Repair.State
import qualified Repair.Term as RT
import qualified Repair.Vernacular as RV
import           Script
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Term
import           Term.Variable
import qualified Typing.GlobalEnvironment as GE
-- import qualified Typing.LocalContext as LC
import           Utils
import           Vernacular

propagate :: DI.Diff Raw.Raw -> DGE.Diff Raw.Raw -> DGE.Diff Raw.Raw
propagate DI.Same = id
propagate (DI.Change δn δps δis δcs) = do
  -- this assumes that the following happens
  -- 1. the inductive type is added
  -- 2. the type of each constructor is added
