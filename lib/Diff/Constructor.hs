{-# LANGUAGE FlexibleContexts #-}

module Diff.Constructor
  ( Diff(..)
  , patch
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception

import qualified Diff.Atom as DA
import qualified Diff.List as DL
import qualified Diff.Pair as DP
import qualified Diff.Term as DT
import           Inductive.Inductive
-- import           PrettyPrinting.PrettyPrintable
-- import           PrettyPrinting.PrettyPrintableUnannotated
-- import           StandardLibrary
import           Term.Binder
import           Term.Term
import           Term.Variable

type Term α = TypeX α Variable
type BoundTerm α = (Binder Variable, Term α)

data Diff α
  = Same
  | Modify
    (DA.Diff Variable)
    (DL.Diff (BoundTerm α) (DP.Diff (DA.Diff (Binder Variable)) (DT.Diff α)))
    (DL.Diff (Term α)      (DT.Diff α))
  deriving (Show)

-- | Note: `patch` does not replace the reference to `Inductive` in the constructor. The caller must
-- | finish tying the knot!
patch ::
  Member (Exc String) r =>
  Constructor α Variable ->
  Diff α ->
  Eff r (Constructor α Variable)
patch c@(Constructor ind n ps is) d = case d of
  Same              -> return c
  Modify δn δps δis -> do
    n'  <- DA.patch n δn
    ps' <- DL.patch (DP.patch DA.patch DT.patch) ps δps
    is' <- DL.patch DT.patch is δis
    return $ Constructor ind n' ps' is'

-- test :: String
-- test =
--   let Inductive i ips _ [_, cons] =  inductiveVec in
--   let d = Change (DA.Change (Variable "snoc")) (DL.Keep (DL.Permute [1, 0] DL.Same)) DL.Same in
--   case run . runError $ patch cons d of
--     Left e -> e
--     Right (Constructor n ps is) ->
--       let ct = rawConstructorType i ips ps is in
--       printf "%s : %s" (prettyStr n) (prettyStrU ct)
