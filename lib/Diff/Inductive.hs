{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Diff.Inductive
  ( Diff(..)
  , δinductiveRawType
  , patch
  ) where

import           Control.Monad.Fix
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace

import qualified Diff.Atom as DA
import qualified Diff.Constructor as DC
import qualified Diff.List as DL
import qualified Diff.Pair as DP
import qualified Diff.Term as DT
import           Inductive.Inductive
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Term
import           Term.Variable

type Term α = TypeX α Variable
type BoundTerm α = (Binder Variable, Term α)

data Diff α
  = Same
  | Modify
    (DA.Diff Variable)
    (DL.Diff (BoundTerm α)            (DP.Diff (DA.Diff (Binder Variable)) (DT.Diff α)))
    (DL.Diff (BoundTerm α)            (DP.Diff (DA.Diff (Binder Variable)) (DT.Diff α)))
    (DL.Diff (Constructor α Variable) (DC.Diff α))
  deriving (Show)

patch ::
  ( Member (Exc String) r
  , Member Trace r
  , Show α
  ) =>
  Inductive α Variable -> Diff α -> Eff r (Inductive α Variable)
patch ind@(Inductive n ps is cs) = \case
  Same                  -> return ind
  Modify δn δps δis δcs -> do
    n'  <- DA.patch n δn
    ps' <- DL.patch (DP.patch DA.patch DT.patch) ps δps
    is' <- DL.patch (DP.patch DA.patch DT.patch) is δis
    cs' <- DL.patch DC.patch cs δcs -- note: the constructors still refer to the old inductive!
    return $ fix $ \ind' -> Inductive n' ps' is' $ map (updateConstructorInd ind') cs'
      where
        updateConstructorInd ind' (Constructor _ cn cps cis) = Constructor ind' cn cps cis

-- so, the output type is:
-- Pi p0 (Pi p1 (Pi p2 (Pi i0 (Pi i1 (Pi i2 Type))))))
-- and becomes:
-- Pi p0' (Pi p1' (Pi i0' (Pi i1' Type))))
-- δps will tell us how to update the p-telescope
-- δis will tell us how to update the i-telescope
-- Problem: if δps is Same, we can't return Same because maybe Raw.Rawis

δinductiveRawType ::
  Int ->
  (DL.Diff (BoundTerm Raw.Raw) (DP.Diff (DA.Diff (Binder Variable)) (DT.Diff Raw.Raw))) ->
  Int ->
  (DL.Diff (BoundTerm Raw.Raw) (DP.Diff (DA.Diff (Binder Variable)) (DT.Diff Raw.Raw))) ->
  DT.Diff Raw.Raw
δinductiveRawType nPs δps nIs δis =
  processPs nPs (processIs nIs δis) δps

  where
    -- processIs :: Int -> DL.Diff (BoundTerm Raw.Raw) (DA.Diff (BoundTerm Raw.Raw)) -> DT.Diff Raw.Raw
    processIs n = \case
      DL.Same -> nCpyPis n DT.Same
      DL.Insert (b, τ) δ -> DT.InsPi () (DT.Replace τ) b $ processIs n δ
      DL.Modify _δt     _δ -> error "TODO"
      _ -> error "TODO"

    processPs n base = \case
      DL.Same -> nCpyPis n base
      _ -> error "TODO"

    nCpyPis 0 base         = base
    nCpyPis n _    | n < 0 = error "nCpyPis: n became negative!"
    nCpyPis n base         = DT.CpyPi DT.Same DA.Same $ nCpyPis (n - 1) base
