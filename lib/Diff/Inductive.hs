{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Diff.Inductive
  ( Diff(..)
  , patch
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception

import qualified Diff.Atom as DA
import qualified Diff.Constructor as DC
import qualified Diff.List as DL
import           Inductive.Constructor
import           Inductive.Inductive
import           Term.Binder
import           Term.Term
import           Term.Variable

type Term α = TypeX α Variable
type Parameter α = (Binder Variable, Term α)

data Diff α
  = Same
  | Change
    (DA.Diff Variable)
    (DL.Diff (Parameter α)            (DA.Diff (Parameter α)))
    (DL.Diff (Term α)                 (DA.Diff (Term α)))
    (DL.Diff (Constructor α Variable) (DC.Diff α))
  deriving (Show)

patch ::
  Member (Exc String) r =>
  Inductive α Variable -> Diff α -> Eff r (Inductive α Variable)
patch ind@(Inductive n ps is cs) = \case
  Same                  -> return ind
  Change δn δps δis δcs -> do
    n'  <- DA.patch n δn
    ps' <- DL.patch DA.patch ps δps
    is' <- DL.patch DA.patch is δis
    cs' <- DL.patch DC.patch cs δcs
    return $ Inductive n' ps' is' cs'
