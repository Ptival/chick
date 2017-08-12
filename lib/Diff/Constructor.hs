{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Diff.Constructor
  ( Diff(..)
  , patch
  , δconstructorRawType
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Text.PrettyPrint.Annotated.WL

import qualified Diff.Atom as DA
import qualified Diff.List as DL
import qualified Diff.Pair as DP
import qualified Diff.Term as DT
import           Inductive.Inductive
import           PrettyPrinting.PrettyPrintable
-- import           PrettyPrinting.PrettyPrintableUnannotated
import           PrettyPrinting.Term
-- import           StandardLibrary
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Term
import           Term.Variable
import           Text.Printf

type Term α = TypeX α Variable
type BoundTerm α = (Binder Variable, Term α)

data Diff α
  = Same
  | Modify
    (DA.Diff Variable)
    (DL.Diff (BoundTerm α) (DP.Diff (DA.Diff (Binder Variable)) (DT.Diff α)))
    (DL.Diff (Term α)      (DT.Diff α))
  deriving (Show)

instance PrettyPrintable (Diff α) where
  prettyDoc Same              = text "Same"
  prettyDoc (Modify δ1 δ2 δ3) = fillSep [ text "Modify", prettyDoc δ1, prettyDoc δ2, prettyDoc δ3 ]

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
  (DL.Diff (BoundTerm Raw.Raw) (DP.Diff (DA.Diff (Binder Variable)) (DT.Diff Raw.Raw))) ->
  Int ->
  (DL.Diff (Raw.Term Variable) (DT.Diff Raw.Raw)) ->
  DT.Diff Raw.Raw
δconstructorRawType prefix nPs δps nIs δis =
  processPs nPs (processIs nIs prefix δis) δps

  where
    processIndPs n = \case
      _ -> error "TODO: processIndPs"

    processIs ::
      Int ->
      DT.Diff Raw.Raw ->
      DL.Diff (Raw.Term Variable) (DT.Diff Raw.Raw) ->
      DT.Diff Raw.Raw
    processIs n base = \case
      DL.Insert t δ -> DT.InsApp () (DT.Replace t) $ processIs n base δ
      DL.Same -> nCpyApps n DT.Same
      δ -> error $ printf "TODO: processIs %s" (show δ)

    processPs ::
      Int ->
      DT.Diff Raw.Raw ->
      DL.Diff (BoundTerm Raw.Raw) (DP.Diff (DA.Diff (Binder Variable)) (DT.Diff Raw.Raw)) ->
      DT.Diff Raw.Raw
    processPs n base = \case
      DL.Insert (b, t) δ -> DT.InsPi () (DT.Replace t) b (processPs n base δ)
      DL.Keep δ -> DT.CpyPi DT.Same DA.Same (processPs (n-1) base δ)
      DL.Modify DP.Same δ -> DT.CpyPi DT.Same DA.Same (processPs (n-1) base δ)
      DL.Modify (DP.Modify δl δr) δ -> DT.CpyPi δr δl (processPs (n-1) base δ)
      DL.Same -> nCpyPis n base
      δ -> error $ printf "TODO: processPs %s" (show δ)

    nCpyApps 0 base         = base
    nCpyApps n base | n < 0 = error "nCpyApps: n became negative!"
    nCpyApps n base         = DT.CpyApp (nCpyApps (n-1) base) DT.Same

    nCpyPis 0 base         = base
    nCpyPis n _    | n < 0 = error "nCpyPis: n became negative!"
    nCpyPis n base         = DT.CpyPi DT.Same DA.Same $ nCpyPis (n - 1) base
