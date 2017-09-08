{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff.Inductive
  ( Δis
  , Δps
  , Diff(..)
  , δinductiveRawType
  , δinductiveRawConstructorPrefix
  -- , instantiateΔis
  , patch
  ) where

import           Control.Monad.Fix
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
-- import           Data.Bifunctor
import           Text.PrettyPrint.Annotated.WL

import qualified Diff.Atom as DA
import qualified Diff.Constructor as DC
import qualified Diff.List as DL
import qualified Diff.Pair as DP
import qualified Diff.Term as DT
import           Inductive.Inductive
import           PrettyPrinting.PrettyPrintable
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Term
import           Text.Printf

type Term α = TypeX α Variable
-- type BoundTerm α = (Binder Variable, Term α)

type Δn = DA.Diff Variable

type Φp  α = (Variable, Term α)
type Δp  α = DP.Diff (DA.Diff Variable) (DT.Diff α)
type Δps α = DL.Diff (Φp α) (Δp α)

type Φi  α = (Variable, Term α)
type Δi  α = DP.Diff (DA.Diff Variable) (DT.Diff α)
type Δis α = DL.Diff (Φi α) (Δi α)

-- type Φi  α = BoundTerm α
-- type Δi  α = DP.Diff (DA.Diff (Binder Variable)) (DT.Diff α)
-- type Δis α = DL.Diff (Φi α) (Δi α)

-- type Φi'  α = (Variable, Term α)
-- type Δi'  α = DP.Diff (DA.Diff Variable) (DT.Diff α)
-- type Δis' α = DL.Diff (Φi' α) (Δi' α)

{-
instantiateΔis :: Δis α -> Δis' α
instantiateΔis = bimap f g
  where
    f :: Φi α -> Φi' α
    f (b, τ) = (variableFromBinder b, τ) -- TODO: fix this

    g :: Δi α -> Δi' α
    g = _

    variableFromBinder :: Binder Variable -> Variable
    variableFromBinder (Binder b) = case b of
      Nothing -> "TODO"
      Just v  -> v
-}

type Φc  α = Constructor α Variable
type Δc  α = DC.Diff α
type Δcs α = DL.Diff (Φc α) (Δc α)

data Diff α
  = Same
  | Modify Δn (Δps α) (Δis α) (Δcs α)

instance Show α => Show (Diff α) where
  show Same             = "Same"
  show (Modify _ _ _ _) = "Modify _ _ _ _"

instance PrettyPrintable (Diff α) where
  prettyDoc = \case
    Same               -> text "Same"
    Modify δ1 δ2 δ3 δ4 -> fillSep [ text "Modify", go δ1 , go δ2, go δ3, go δ4 ]

    where
      go :: PrettyPrintable a => a -> Doc ()
      go = parens . prettyDoc

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
  Δps Raw.Raw ->
  Int ->
  Δis Raw.Raw ->
  DT.Diff Raw.Raw
δinductiveRawType nPs δps nIs δis =
  processPs nPs (processIs nIs δis) δps

  where

    processIs :: Int -> Δis Raw.Raw -> DT.Diff Raw.Raw
    processIs n = \case
      DL.Same -> nCpyPis n DT.Same
      DL.Insert (b, τ) δ ->
        DT.InsPi () (DT.Replace τ) (Binder (Just b)) $ processIs n δ
      DL.Modify _δt     _δ -> error "TODO"
      _ -> error "TODO"

    processPs :: Int -> DT.Diff Raw.Raw -> Δps Raw.Raw -> DT.Diff Raw.Raw
    processPs n base = \case
      DL.Same -> nCpyPis n base
      _ -> error "TODO"

    nCpyPis 0 base         = base
    nCpyPis n _    | n < 0 = error "nCpyPis: n became negative!"
    nCpyPis n base         = DT.CpyPi DT.Same DA.Same $ nCpyPis (n - 1) base

δinductiveRawConstructorPrefix ::
  DA.Diff Variable ->
  Int ->
  Δps Raw.Raw ->
  DT.Diff Raw.Raw
δinductiveRawConstructorPrefix δindName nPs δps =
  processPs nPs (DT.CpyVar δindName) δps

  where

    processPs n base = \case
      DL.Same -> nCpyApps n base
      δ -> error $ printf "TODO: %s" (show δ)

    nCpyApps 0 base         = base
    nCpyApps n _    | n < 0 = error "nCpyApps: n became negative!"
    nCpyApps n base         = DT.CpyApp (nCpyApps (n - 1) base) DT.Same
