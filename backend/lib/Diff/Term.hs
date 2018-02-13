{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Diff.Term
  ( BranchDiff
  , Diff(..)
  , extractApps
  , extractLams
  , extractPi
  , extractPis
  , extractSomeApps
  , extractSomeLams
  , extractSomePis
  , mkLams
  , mkPis
  , nCpyApps
  , nCpyPis
  , nInsertApps
  , nRemoveApps
  , patch
  , patchMaybe
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Data.Aeson
import           Data.Either.Combinators
import           GHC.Generics
import           Text.Printf
import           Text.PrettyPrint.Annotated.WL

import qualified Diff.Atom as DA
import qualified Diff.List as DL
import qualified Diff.Triple as D3
import           Diff.Utils
import           PrettyPrinting.Term ()
import           PrettyPrinting.PrettyPrintable
import           Term.Binder
import           Term.Term
import           Utils

type BranchDiff α =
  D3.Diff
  (DA.Diff Variable)
  (DL.Diff (Binder Variable) (DA.Diff (Binder Variable)))
  (Diff α)

data Diff α
  = Same
  | Replace    (TermX α Variable)
  | CpyApp     (Diff α) (Diff α)
  | CpyLam     (DA.Diff (Binder Variable)) (Diff α)
  | CpyMatch   (Diff α) (DL.Diff (Branch α Variable) (BranchDiff α))
  | CpyPi      (Diff α) (DA.Diff (Binder Variable)) (Diff α)
  | CpyVar     (DA.Diff Variable)
  | InsApp     α (Diff α) (Diff α)
  | InsLam     α (Binder Variable) (Diff α)
  | InsPi      α (Diff α) (Binder Variable) (Diff α)
  -- NOTE: the semantics of PermutApps are now s.t. running permutation
  -- [2, 0, 1] with term (f a b c d e) should yield term (f a b e c d)
  | PermutApps [Int] (Diff α)
  | PermutLams [Int] (Diff α)
  | PermutPis  [Int] (Diff α)
  | RemoveApp  (Diff α) -- removes the right term
  | RemoveLam  (Diff α)
  | RemovePi   (Diff α) -- removes the left term and binder
  deriving (Eq, Generic, Show)

instance PrettyPrintable (Diff α) where
  prettyDoc = \case
    Same              -> text "Same"
    Replace t         -> fillSep [ text "Replace",    go t ]
    CpyApp δ1 δ2      -> fillSep [ text "CpyApp",     go δ1, go δ2 ]
    CpyLam δ1 δ2      -> fillSep [ text "CpyLam",     go δ1, go δ2 ]
    CpyMatch δ1 δ2    -> fillSep [ text "CpyMatch",   go δ1, go δ2 ]
    CpyPi  δ1 δ2 δ3   -> fillSep [ text "CpyPi",      go δ1, go δ2, go δ3 ]
    CpyVar δ1         -> fillSep [ text "CpyVar",     go δ1 ]
    InsApp _ δ1 δ2    -> fillSep [ text "InsApp",     go δ1, go δ2 ]
    InsLam _ δ1 δ2    -> fillSep [ text "InsLam",     go δ1, go δ2 ]
    InsPi  _ δ1 δ2 δ3 -> fillSep [ text "InsPi",      go δ1, go δ2, go δ3 ]
    PermutApps p δ1   -> fillSep [ text "PermutApp",  (text $ show p), go δ1 ]
    PermutLams p δ1   -> fillSep [ text "PermutLam",  (text $ show p), go δ1 ]
    PermutPis  p δ1   -> fillSep [ text "PermutPi",   (text $ show p), go δ1 ]
    RemoveApp δ1      -> fillSep [ text "RemoveApp",  go δ1 ]
    RemoveLam δ1      -> fillSep [ text "RemoveApp",  go δ1 ]
    RemovePi δ1       -> fillSep [ text "RemovePi",   go δ1 ]

    where
      go :: PrettyPrintable a => a -> Doc ()
      go = parens . prettyDoc

instance ToJSON α => ToJSON (Diff α) where

patchBranch ::
  ( Member (Exc String) r
  , Member Trace r
  , Show α
  ) =>
  Branch α Variable -> BranchDiff α -> Eff r (Branch α Variable)
patchBranch b D3.Same = return b
patchBranch b (D3.Modify δctor δargs δbody) = do
  let (ctor, args, body) = unpackBranch b
  ctor' <- DA.patch ctor δctor
  args' <- DL.patch DA.patch args δargs
  body' <- patch body δbody
  return $ packBranch (ctor', args', body')

patch ::
  ( Member (Exc String) r
  , Member Trace r
  , Show α
  ) =>
  TermX α Variable -> Diff α -> Eff r (TermX α Variable)
patch term δterm =

  let exc (s :: String) = throwExc $ printf "Diff.Term/patch: %s" s in

  -- trace (printf "Diff.Term/patch:(%s, %s)" (preview term) (show δterm)) >>

  case (term, δterm) of

  (_, Same) -> return term

  (_, Replace t') -> return t'

  (App a t1 t2, CpyApp d1 d2) -> App a <$> patch t1 d1 <*> patch t2 d2
  (_, CpyApp _ _) -> exc "CpyApp, not an App"

  (Lam a bt, CpyLam db dt) ->
    let (b, t') = unscopeTerm bt in
    Lam a <$> (abstractBinder <$> DA.patch b db <*> patch t' dt)
  (_, CpyLam _ _) -> exc "CpyLam, not a Lam"

  (Match a d bs, CpyMatch δd δbs) ->
    Match a <$> patch d δd <*> DL.patch patchBranch bs δbs
  (_, CpyMatch _ _) -> exc "CpyMatch, not a Match"

  (Pi a τ1 bτ2, CpyPi d1 db d2) ->
    let (b, τ2) = unscopeTerm bτ2 in
    Pi a <$> patch τ1 d1 <*> (abstractBinder <$> DA.patch b db <*> patch τ2 d2)
  (_, CpyPi _ _ _) -> exc "CpyPi, not a Pi"

  (Var a v, CpyVar δv) -> Var a <$> DA.patch v δv
  (_, CpyVar _) -> exc $ printf "CpyVar, not a Var: %s" (prettyStr term)

  (_, InsApp a d1 d2) -> App a <$> patch term d1 <*> patch term d2

  (_, InsLam a b d1) -> Lam a . abstractBinder b <$> patch term d1

  (_, InsPi a d1 b d2) ->
    Pi a <$> patch term d1 <*> (abstractBinder b <$> patch term d2)

  (_, PermutApps p δ') -> do
    (fun, args) <- extractSomeApps (length p) term
    patch (mkApps fun (permute p args)) δ'

  (_, PermutLams p d') -> do
    (lams, rest) <- extractSomeLams (length p) term
    patch (mkLams (permute p lams) rest) d'

  (_, PermutPis p d') -> do
    (pis, rest) <- extractSomePis (length p) term
    patch (mkPis (permute p pis) rest) d'

  (App _ t1 _, RemoveApp δ) -> patch t1 δ
  (_, RemoveApp _) -> exc "RemoveApp: not an App"

  (Lam _ bt, RemoveLam δ) ->
    let (_, t) = unscopeTerm bt in
    patch t δ
  (_, RemoveLam _) -> exc "RemoveLam: not a Lam"

  (Pi _ _ bτ2, RemovePi δ) ->
    let (_, τ2) = unscopeTerm bτ2 in
    patch τ2 δ
  (_, RemovePi _) -> exc "RemovePi: not a Pi"

nCpyApps :: Int -> Diff α -> Diff α
nCpyApps 0 base         = base
nCpyApps n _    | n < 0 = error "nCpyApps: n became negative!"
nCpyApps n base         = CpyApp (nCpyApps (n-1) base) Same

nCpyPis :: Int -> Diff α -> Diff α
nCpyPis 0 base         = base
nCpyPis n _    | n < 0 = error "nCpyPis: n became negative!"
nCpyPis n base         = CpyPi Same DA.Same $ nCpyPis (n - 1) base

nInsertApps :: Int -> (α, TermX α Variable) -> Diff α -> Diff α
nInsertApps 0 _      base         = base
nInsertApps n _      _    | n < 0 = error "nRemoveApps: n became negative!"
nInsertApps n (α, a) base         =
  InsApp α (nInsertApps (n - 1) (α, a) base) (Replace a)

nRemoveApps :: Int -> Diff α -> Diff α
nRemoveApps 0 base         = base
nRemoveApps n _    | n < 0 = error "nRemoveApps: n became negative!"
nRemoveApps n base         = RemoveApp $ nRemoveApps (n - 1) base

patchMaybe :: Show α => TermX α Variable -> Diff α -> Maybe (TermX α Variable)
patchMaybe t d = rightToMaybe @String . skipTrace . runError $ patch t d
