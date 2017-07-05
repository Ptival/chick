{-# LANGUAGE FlexibleContexts #-}

module Diff.Constructor
  ( Diff(..)
  , patch
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Text.Printf

import qualified Diff.Atom as DA
import qualified Diff.List as DL
import qualified Diff.Term as DT
import qualified Diff.Variable as DV
import           Inductive.Constructor
import           Inductive.Inductive
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           StandardLibrary
import           Term.Binder
import           Term.Term
import qualified Term.Raw as Raw
import           Term.Variable

type Term α = TypeX α Variable
type Parameter α = (Binder Variable, Term α)

data Diff α
  = Same
  | Change
    (DA.Diff Variable)
    (DL.Diff (Parameter α) (DA.Diff (Parameter α)))
    (DL.Diff (Term α)      (DA.Diff (Term α)))

patch ::
  Member (Exc String) r =>
  Constructor α Variable -> Diff α -> Eff r (Constructor α Variable)
patch c@(Constructor n ps is) d = case d of
  Same              -> return c
  Change dn dps dis -> do
    n'  <- DA.patch n dn
    ps' <- DL.patch DA.patch ps dps
    is' <- DL.patch DA.patch is dis
    return $ Constructor n' ps' is'

test :: String
test =
  let Inductive i ips _ [_, cons] =  inductiveVec in
  let d = Change (DA.Change (Variable "snoc")) (DL.Keep (DL.Flip DL.Same)) DL.Same in
  case run . runError $ patch cons d of
    Left e -> e
    Right (Constructor n ps is) ->
      let ct = rawConstructorType i ips ps is in
      printf "%s : %s" (prettyStr n) (prettyStrU ct)
