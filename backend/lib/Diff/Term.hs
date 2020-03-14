{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Diff.Term
  ( BinderDiff
  , BindersDiff
  , BranchDiff
  , BranchesDiff
  , Diff(..)
  , VariableDiff
  , VariablesDiff
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
  , nCpyLams
  , nCpyPis
  , nInsertApps
  , nRemoveApps
  , patch
  , patchMaybe
  ) where

import           Data.Aeson                     ( ToJSON )
import           Data.Either.Combinators        ( rightToMaybe )
import qualified Data.Text.Prettyprint.Doc      as Doc
import           GHC.Generics                   ( Generic )
import           Polysemy                       ( Member, Sem, run )
import           Polysemy.Error                 ( Error, runError, throw )
import           Polysemy.Trace                 ( Trace, ignoreTrace )
import           Text.Printf                    ( printf )

import qualified Diff.Atom as ΔA
import qualified Diff.List as ΔL
import qualified Diff.Maybe as ΔM
import qualified Diff.Pair as Δ2
import qualified Diff.Triple as Δ3
import           Diff.Utils
import           Language                       ( Language(Chick) )
import           PrettyPrinting.Term            ( )
import           PrettyPrinting.PrettyPrintable
import           Term.Binder
import           Term.Term
import           Utils

type VariableDiff  = ΔA.Diff Variable
type VariablesDiff = ΔL.Diff Variable VariableDiff

type BinderDiff  ν = ΔA.Diff (Binder ν)
type BindersDiff ν = ΔL.Diff (Binder ν) (BinderDiff ν)

type GuardAndBodyDiff α = Δ2.Diff (ΔM.Diff (TermX α Variable) (Diff α)) (Diff α)

type BranchDiff   α = Δ3.Diff VariableDiff (BindersDiff Variable) (GuardAndBodyDiff α)
type BranchesDiff α = ΔL.Diff (Branch α Variable) (BranchDiff α)

data Diff α
  = Same
  | Replace    (TermX α Variable)
  | CpyApp     (Diff α) (Diff α)
  | CpyLam     (BinderDiff Variable) (Diff α)
  | CpyMatch   (Diff α) (BranchesDiff α)
  | CpyPi      (Diff α) (BinderDiff Variable) (Diff α)
  | CpyVar     (ΔA.Diff Variable)
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

instance
  ( PrettyPrintable l (Binder Variable)
  , PrettyPrintable l (Branch α Variable)
  , PrettyPrintable l (TermX α Variable)
  , PrettyPrintable l Variable
  ) => PrettyPrintable l (Diff α) where
  prettyDoc = \case
    Same              -> "Same"
    Replace t         -> Doc.fillSep [ "Replace",    go t ]
    CpyApp δ1 δ2      -> Doc.fillSep [ "CpyApp",     go δ1, go δ2 ]
    CpyLam δ1 δ2      -> Doc.fillSep [ "CpyLam",     go δ1, go δ2 ]
    CpyMatch δ1 δ2    -> Doc.fillSep [ "CpyMatch",   go δ1, go δ2 ]
    CpyPi  δ1 δ2 δ3   -> Doc.fillSep [ "CpyPi",      go δ1, go δ2, go δ3 ]
    CpyVar δ1         -> Doc.fillSep [ "CpyVar",     go δ1 ]
    InsApp _ δ1 δ2    -> Doc.fillSep [ "InsApp",     go δ1, go δ2 ]
    InsLam _ δ1 δ2    -> Doc.fillSep [ "InsLam",     go δ1, go δ2 ]
    InsPi  _ δ1 δ2 δ3 -> Doc.fillSep [ "InsPi",      go δ1, go δ2, go δ3 ]
    PermutApps p δ1   -> Doc.fillSep [ "PermutApp",  (Doc.pretty $ show p), go δ1 ]
    PermutLams p δ1   -> Doc.fillSep [ "PermutLam",  (Doc.pretty $ show p), go δ1 ]
    PermutPis  p δ1   -> Doc.fillSep [ "PermutPi",   (Doc.pretty $ show p), go δ1 ]
    RemoveApp δ1      -> Doc.fillSep [ "RemoveApp",  go δ1 ]
    RemoveLam δ1      -> Doc.fillSep [ "RemoveApp",  go δ1 ]
    RemovePi δ1       -> Doc.fillSep [ "RemovePi",   go δ1 ]

    where
      go :: PrettyPrintable l a => a -> Doc.Doc ()
      go = Doc.parens . (prettyDoc @l)

instance ToJSON α => ToJSON (Diff α) where

patchGuardAndBody ::
  ( Member (Error String) r
  , Member Trace r
  , Show α
  ) =>
  GuardAndBody (TermX α) Variable -> GuardAndBodyDiff α ->
  Sem r (GuardAndBody (TermX α) Variable)
patchGuardAndBody gb Δ2.Same = return gb
patchGuardAndBody gb (Δ2.Modify δguard δbody) = do
  guard' <- ΔM.patch patch (branchGuard gb) δguard
  body'  <- patch          (branchBody  gb)  δbody
  return $ GuardAndBody guard' body'

patchBranch ::
  ( Member (Error String) r
  , Member Trace r
  , Show α
  ) =>
  Branch α Variable -> BranchDiff α -> Sem r (Branch α Variable)
patchBranch b Δ3.Same = return b
patchBranch b (Δ3.Modify δctor δargs δguardbody) = do
  let (ctor, args, guardbody) = unpackBranch b
  ctor' <- ΔA.patch ctor δctor
  args' <- ΔL.patch ΔA.patch args δargs
  guardbody' <- patchGuardAndBody guardbody δguardbody
  return $ packBranch (ctor', args', guardbody')

patch ::
  ( Member (Error String) r
  , Member Trace r
  , Show α
  ) =>
  TermX α Variable -> Diff α -> Sem r (TermX α Variable)
patch term δterm =

  let
    exc :: Member (Error String) r => String -> Sem r a
    exc s = throw (printf "Diff.Term/patch: %s" s :: String)
  in

  -- trace (printf "Diff.Term/patch:(%s, %s)" (preview term) (show δterm)) >>

  case (term, δterm) of

  (_, Same) -> return term

  (_, Replace t') -> return t'

  (App a t1 t2, CpyApp d1 d2) -> App a <$> patch t1 d1 <*> patch t2 d2
  (_, CpyApp _ _) -> exc "CpyApp, not an App"

  (Lam a bt, CpyLam db dt) ->
    let (b, t') = unscopeTerm bt in
    Lam a <$> (abstractBinder <$> ΔA.patch b db <*> patch t' dt)
  (_, CpyLam _ _) -> exc "CpyLam, not a Lam"

  (Match a d bs, CpyMatch δd δbs) ->
    Match a <$> patch d δd <*> ΔL.patch patchBranch bs δbs
  (_, CpyMatch _ _) -> exc "CpyMatch, not a Match"

  (Pi a τ1 bτ2, CpyPi d1 db d2) ->
    let (b, τ2) = unscopeTerm bτ2 in
    Pi a <$> patch τ1 d1 <*> (abstractBinder <$> ΔA.patch b db <*> patch τ2 d2)
  (_, CpyPi _ _ _) -> exc "CpyPi, not a Pi"

  (Var a v, CpyVar δv) -> Var a <$> ΔA.patch v δv
  (_, CpyVar _) -> exc $ printf "CpyVar, not a Var: %s" (prettyStr @'Chick term)

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

nCpy :: (Diff α -> Diff α) -> Int -> Diff α -> Diff α
nCpy _ 0 base         = base
nCpy _ n _    | n < 0 = error "nCpy: n became negative!"
nCpy f n base         = f (nCpy f (n-1) base)

nCpyApps :: Int -> Diff α -> Diff α
nCpyApps = nCpy (\ δ -> CpyApp δ Same)

nCpyLams :: Int -> Diff α -> Diff α
nCpyLams = nCpy (\ δ -> CpyLam ΔA.Same δ)

nCpyPis :: Int -> Diff α -> Diff α
nCpyPis = nCpy (\ δ -> CpyPi Same ΔA.Same δ)

nInsertApps :: Int -> (α, TermX α Variable) -> Diff α -> Diff α
nInsertApps 0 _      base         = base
nInsertApps n _      _    | n < 0 = error "nRemoveApps: n became negative!"
nInsertApps n (α, a) base         =
  InsApp α (nInsertApps (n - 1) (α, a) base) (Replace a)

nRemoveApps :: Int -> Diff α -> Diff α
nRemoveApps 0 base         = base
nRemoveApps n _    | n < 0 = error "nRemoveApps: n became negative!"
nRemoveApps n base         = RemoveApp $ nRemoveApps (n - 1) base

patchMaybe ::
  Show α =>
  TermX α Variable -> Diff α -> Maybe (TermX α Variable)
patchMaybe t d = rightToMaybe @String . run . ignoreTrace . runError $ patch t d
