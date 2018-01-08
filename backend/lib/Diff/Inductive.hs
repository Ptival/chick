{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff.Inductive
  ( Δc
  , Δcs
  , Δii
  , Δiis
  , Δip
  , Δips
  , Diff(..)
  , δconstructorRawType
  , δinductiveRawType
  , δinductiveRawConstructorPrefix
  -- , instantiateΔis
  , patch
  , patchIndex
  , patchParameter
  ) where

import           Control.Monad.Fix
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Text.PrettyPrint.Annotated.WL

import qualified Diff.Atom as DA
import qualified Diff.Constructor as DC
import qualified Diff.List as DL
import           Diff.ListFoldUtils
import qualified Diff.Term as DT
import qualified Diff.Triple as D3
import           Inductive.Inductive
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.Universe ()
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Term
import           Term.Universe
import           Text.Printf

-- type Term α = TypeX α Variable
-- type BoundTerm α = (Binder Variable, Term α)

type Δn = DA.Diff Variable

type Δip  α = D3.Diff (DA.Diff α) (DA.Diff Variable) (DT.Diff α)
type Δips α = DL.Diff (Φip α Variable) (Δip α)

type Δii  α = D3.Diff (DA.Diff α) (DA.Diff Variable) (DT.Diff α)
type Δiis α = DL.Diff (Φii α Variable) (Δii α)

type Δu = DA.Diff Universe

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

type Δc  α = DC.Diff α
type Δcs α = DL.Diff (Constructor α Variable) (Δc α)

data Diff α
  = Same
  | Modify
    { δn   :: Δn
    , δips :: Δips α
    , δiis :: Δiis α
    , δu   :: Δu
    , δcs  :: Δcs α
    }
  deriving (Show)

instance PrettyPrintable α => PrettyPrintable (Diff α) where
  prettyDoc = \case
    Same               -> text "Same"
    Modify δ1 δ2 δ3 δ4 δ5 -> fillSep [ text "Modify", go δ1 , go δ2, go δ3, go δ4, go δ5 ]

    where
      go :: PrettyPrintable a => a -> Doc ()
      go = parens . prettyDoc

patchParameter ::
  ( Member (Exc String) r
  , Member Trace r
  , Show α
  ) =>
  Φip α Variable -> Δip α -> Eff r (Φip α Variable)
patchParameter = D3.patch DA.patch DA.patch DT.patch

patchIndex ::
  ( Member (Exc String) r
  , Member Trace r
  , Show α
  ) =>
  Φip α Variable -> Δip α -> Eff r (Φip α Variable)
patchIndex = D3.patch DA.patch DA.patch DT.patch

patch ::
  ( Member (Exc String) r
  , Member Trace r
  , PrettyPrintable α
  , Show α
  ) =>
  Inductive α Variable -> Diff α -> Eff r (Inductive α Variable)
patch ind@(Inductive n ps is u cs) = \case
  Same                     -> return ind
  Modify δn δps δis δu δcs -> do
    n'  <- DA.patch n δn
    ps' <- DL.patch patchParameter ps δps
    is' <- DL.patch patchIndex is δis
    u'  <- DA.patch u δu
    cs' <- DL.patch DC.patch cs δcs -- δcs -- note: the constructors still refer to the old inductive!
    return $ fix $ \ind' -> Inductive n' ps' is' u' $ map (updateConstructorInd ind') cs'
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
  Δips Raw.Raw ->
  Int ->
  Δiis Raw.Raw ->
  DT.Diff Raw.Raw
δinductiveRawType nPs δps nIs δis =
  processPs nPs (processIs nIs δis) δps

  where

    processIs :: Int -> Δiis Raw.Raw -> DT.Diff Raw.Raw
    processIs n = \case
      DL.Same -> DT.nCpyPis n DT.Same
      DL.Insert ((), b, τ) δ ->
        DT.InsPi () (DT.Replace τ) (Binder (Just b)) $ processIs n δ
      DL.Modify _δt     _δ -> error "TODO"
      _ -> error "TODO"

    processPs :: Int -> DT.Diff Raw.Raw -> Δips Raw.Raw -> DT.Diff Raw.Raw
    processPs n base = \case
      DL.Same -> DT.nCpyPis n base
      _ -> error "TODO"

δinductiveRawConstructorPrefix ::
  DA.Diff Variable ->
  Int ->
  Δips Raw.Raw ->
  DT.Diff Raw.Raw
δinductiveRawConstructorPrefix δindName nPs δps =
  processPs nPs (DT.CpyVar δindName) δps

  where

    processPs n base = \case
      DL.Same -> DT.nCpyApps n base
      δ -> error $ printf "TODO: %s" (show δ)

δconstructorRawType ::
  DT.Diff Raw.Raw ->
  Φips Raw.Raw Variable -> Δips Raw.Raw ->
  Φcps Raw.Raw Variable -> DC.Δcps Raw.Raw ->
  Φcis Raw.Raw Variable -> DC.Δcis Raw.Raw ->
  DT.Diff Raw.Raw
δconstructorRawType prefix ips δips cps δcps cis δcis =
  -- foldrWith mkPi $ foldrWith mkApp $ foldrWith (mkApp . fst)

  δquantifyVariables ips δips
  $ δquantifyVariables cps δcps
  $ δapplyTerms cis δcis
  $ prefix

  -- WAS:
  -- processPs nPs (processIs nIs prefix δis) δps

  -- where

  --   processIs ::
  --     Int ->
  --     DT.Diff Raw.Raw ->
  --     DL.Diff (Raw.Term Variable) (DT.Diff Raw.Raw) ->
  --     DT.Diff Raw.Raw
  --   processIs n base = \case
  --     DL.Insert t δ -> DT.InsApp () (processIs n base δ) (DT.Replace t)
  --     DL.Same -> DT.nCpyApps n base
  --     δ -> error $ printf "TODO: processIs %s" (show δ)

  --   processPs ::
  --     Int ->
  --     DT.Diff Raw.Raw ->
  --     Δcps Raw.Raw ->
  --     DT.Diff Raw.Raw
  --   processPs n base = \case
  --     DL.Insert (b, t) δ ->
  --       DT.InsPi () (DT.Replace t) (Binder (Just b)) (processPs n base δ)
  --     DL.Keep δ ->
  --       DT.CpyPi DT.Same DA.Same (processPs (n-1) base δ)
  --     DL.Modify DP.Same δ ->
  --       DT.CpyPi DT.Same DA.Same (processPs (n-1) base δ)
  --     DL.Modify (DP.Modify δl δr) δ ->
  --       DT.CpyPi δr (DB.fromΔVariable δl) (processPs (n-1) base δ)
  --     DL.Same ->
  --       DT.nCpyPis n base
  --     δ -> error $ printf "TODO: processPs %s" (show δ)
