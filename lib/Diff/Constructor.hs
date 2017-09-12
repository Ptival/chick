{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Diff.Constructor
  ( Δcn
  , Δci
  , Δcis
  , Δcp
  , Δcps
  , Diff(..)
  , patch
  , δconstructorRawType
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Text.PrettyPrint.Annotated.WL

import qualified Diff.Atom as DA
import qualified Diff.Binder as DB
import qualified Diff.List as DL
import qualified Diff.Pair as DP
import qualified Diff.Term as DT
import           Inductive.Inductive
import           PrettyPrinting.PrettyPrintable
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Term
import           Text.Printf

type Δcn = DA.Diff Variable

type Δcp α = DP.Diff (DA.Diff Variable) (DT.Diff α)
type Δcps α = DL.Diff (Φcp α Variable) (Δcp α)

type Δci α = DT.Diff α
type Δcis α = DL.Diff (Φci α Variable) (Δci α)

data Diff α
  = Same
  | Modify Δcn (Δcps α) (Δcis α)
  deriving (Show)

instance PrettyPrintable (Diff α) where
  prettyDoc Same              = text "Same"
  prettyDoc (Modify δ1 δ2 δ3) =
    fillSep [ text "Modify", prettyDoc δ1, prettyDoc δ2, prettyDoc δ3 ]

-- | Note: `patch` does not replace the reference to `Inductive` in the constructor. The caller must
-- | finish tying the knot!
patch ::
  ( Member (Exc String) r
  , Member Trace r
  , Show α
  ) =>
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

δconstructorRawType ::
  DT.Diff Raw.Raw ->
  Int ->
  Δcps Raw.Raw ->
  Int ->
  Δcis Raw.Raw ->
  DT.Diff Raw.Raw
δconstructorRawType prefix nPs δps nIs δis =
  processPs nPs (processIs nIs prefix δis) δps

  where

    processIs ::
      Int ->
      DT.Diff Raw.Raw ->
      DL.Diff (Raw.Term Variable) (DT.Diff Raw.Raw) ->
      DT.Diff Raw.Raw
    processIs n base = \case
      DL.Insert t δ -> DT.InsApp () (processIs n base δ) (DT.Replace t)
      DL.Same -> DT.nCpyApps n base
      δ -> error $ printf "TODO: processIs %s" (show δ)

    processPs ::
      Int ->
      DT.Diff Raw.Raw ->
      Δcps Raw.Raw ->
      DT.Diff Raw.Raw
    processPs n base = \case
      DL.Insert (b, t) δ ->
        DT.InsPi () (DT.Replace t) (Binder (Just b)) (processPs n base δ)
      DL.Keep δ ->
        DT.CpyPi DT.Same DA.Same (processPs (n-1) base δ)
      DL.Modify DP.Same δ ->
        DT.CpyPi DT.Same DA.Same (processPs (n-1) base δ)
      DL.Modify (DP.Modify δl δr) δ ->
        DT.CpyPi δr (DB.fromΔVariable δl) (processPs (n-1) base δ)
      DL.Same ->
        DT.nCpyPis n base
      δ -> error $ printf "TODO: processPs %s" (show δ)
