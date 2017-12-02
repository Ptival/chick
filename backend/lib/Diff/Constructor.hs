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
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Text.PrettyPrint.Annotated.WL

import qualified Diff.Atom as DA
import qualified Diff.List as DL
import qualified Diff.Pair as D2
import qualified Diff.Term as DT
import qualified Diff.Triple as D3
import           Inductive.Inductive
import           PrettyPrinting.PrettyPrintable
import           Term.Term

type Δcn = DA.Diff Variable

type Δcp α = D3.Diff (DA.Diff α) (DA.Diff Variable) (DT.Diff α)
type Δcps α = DL.Diff (Φcp α Variable) (Δcp α)

cpPatch ::
  ( Member (Exc String) r
  , Member Trace r
  ) =>
  Φcp α Variable -> Δcp α -> Eff r (Φcp α Variable)
cpPatch = D3.patch DA.patch DA.patch DT.patch

cpsPatch ::
  ( Member (Exc String) r
  , Member Trace r
  , PrettyPrintable α -- huh
  ) =>
  Φcps α Variable -> Δcps α -> Eff r (Φcps α Variable)
cpsPatch = DL.patch cpPatch

type Δci α = D2.Diff (DA.Diff α) (DT.Diff α)
type Δcis α = DL.Diff (Φci α Variable) (Δci α)

ciPatch ::
  ( Member (Exc String) r
  , Member Trace r
  , PrettyPrintable α -- huh
  ) =>
  Φci α Variable -> Δci α -> Eff r (Φci α Variable)
ciPatch = D2.patch DA.patch DT.patch

cisPatch ::
  ( Member (Exc String) r
  , Member Trace r
  , PrettyPrintable α -- huh
  ) =>
  Φcis α Variable -> Δcis α -> Eff r (Φcis α Variable)
cisPatch = DL.patch ciPatch

data Diff α
  = Same
  | Modify Δcn (Δcps α) (Δcis α)
  deriving (Show)

instance PrettyPrintable α => PrettyPrintable (Diff α) where
  prettyDoc Same              = text "Same"
  prettyDoc (Modify δ1 δ2 δ3) =
    fillSep [ text "Modify", prettyDoc δ1, prettyDoc δ2, prettyDoc δ3 ]

-- | Note: `patch` does not replace the reference to `Inductive` in the constructor.
-- | The caller must finish tying the knot!
patch ::
  ( Member (Exc String) r
  , Member Trace r
  , PrettyPrintable α
  , Show α
  ) =>
  Constructor α Variable ->
  Diff α ->
  Eff r (Constructor α Variable)
patch c@(Constructor ind cn cps cis) d = case d of
  Same              -> return c
  Modify δcn δcps δcis -> do
    cn'  <- DA.patch cn δcn
    cps' <- cpsPatch cps δcps
    cis' <- cisPatch cis δcis
    return $ Constructor ind cn' cps' cis'

-- test :: String
-- test =
--   let Inductive i ips _ [_, cons] =  inductiveVec in
--   let d = Change (DA.Change (Variable "snoc")) (DL.Keep (DL.Permute [1, 0] DL.Same)) DL.Same in
--   case run . runError $ patch cons d of
--     Left e -> e
--     Right (Constructor n ps is) ->
--       let ct = rawConstructorType i ips ps is in
--       printf "%s : %s" (prettyStr n) (prettyStrU ct)
