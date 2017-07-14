{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Diff.Term
  ( Diff(..)
  , extractApps
  , extractLams
  , extractPi
  , extractPis
  , extractSomePis
  , extractSomeLams
  , mkLams
  , mkPis
  , patch
  ) where

import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Text.Printf

import qualified Diff.Atom as DA
import           Diff.Utils
import           PrettyPrinting.Term ()
import           PrettyPrinting.PrettyPrintableUnannotated
import           Term.Binder
import           Term.Term
import           Term.Variable

data Diff α
  = Same
  | Change (TermX α Variable)
  | CpyApp (Diff α) (Diff α)
  | CpyLam (DA.Diff (Binder Variable)) (Diff α)
  | CpyPi  (Diff α) (DA.Diff (Binder Variable)) (Diff α)
  | InsApp  α (Diff α) (Diff α)
  | InsLam  α (Binder Variable) (Diff α)
  | InsPi   α (Diff α) (Binder Variable) (Diff α)
  | PermutLams [Int] (Diff α)
  | PermutPis  [Int] (Diff α)
  deriving (Show)

patch ::
  Member (Exc String) r =>
  TermX α Variable -> Diff α -> Eff r (TermX α Variable)
patch t d =
  case d of

    Same -> return t

    Change t' -> return t'

    CpyApp d1 d2 ->
      case t of
        App a t1 t2 -> App a <$> patch t1 d1 <*> patch t2 d2
        _ -> throwExc "patch: CpyApp, not an App"

    CpyLam db dt ->
      case t of
        Lam a bt ->
          let (b, t') = unscopeTerm bt in
          Lam a <$> (abstractBinder <$> DA.patch b db <*> patch t' dt)
        _ -> throwExc "patch: CpyLam, not a Lam"

    CpyPi d1 db d2 ->
      case t of
        Pi a τ1 bτ2 ->
          let (b, τ2) = unscopeTerm bτ2 in
          Pi a <$> patch τ1 d1 <*> (abstractBinder <$> DA.patch b db <*> patch τ2 d2)
        _ -> throwExc "patch: CpyPi, not a Pi"

    InsApp a d1 d2 -> App a <$> patch t d1 <*> patch t d2

    InsLam a b d1 -> Lam a . abstractBinder b <$> patch t d1

    InsPi a d1 b d2 -> Pi a <$> patch t d1 <*> (abstractBinder b <$> patch t d2)

    PermutLams p d' -> do
      (lams, rest) <- extractSomeLams (length p) t
      patch (mkLams (permute p lams) rest) d'

    PermutPis p d' -> do
      (pis, rest) <- extractSomePis (length p) t
      patch (mkPis (permute p pis) rest) d'

-- | `f a b c` becomes `(f, [a, b, c])`
extractApps ::
  Member (Exc String) r =>
  TermX α Variable -> Eff r (TermX α Variable, [(α, TermX α Variable)])
extractApps t = over _2 reverse <$> go t
  where
    go (App a t1 t2) = do
      (f, args) <- go t1
      return $ (f, (a, t2) : args)
    go t1              = return (t1, [])

extractSomeLams ::
  Member (Exc String) r =>
  Int -> TermX α Variable -> Eff r ([(α, Binder Variable)], TermX α Variable)
extractSomeLams 0 t           = return ([], t)
extractSomeLams n (Lam a bt) = do
  let (b, t) = unscopeTerm bt
  (l, rest) <- extractSomeLams (n - 1) t
  return ((a, b) : l, rest)
extractSomeLams _ t              =
  let e :: String = printf "extractLams: not a Lam: %s" (prettyStrU t)
  in throwExc e

extractLams ::
  Member (Exc String) r =>
  TermX α Variable -> Eff r ([(α, Binder Variable)], TermX α Variable)
extractLams = \case
  Lam a bt -> do
    let (b, t) = unscopeTerm bt
    (l, rest) <- extractLams t
    return ((a, b) : l, rest)
  t -> return ([], t)

extractSomePis ::
  Member (Exc String) r =>
  Int -> TermX α Variable -> Eff r ([(α, Binder Variable, TermX α Variable)], TermX α Variable)
extractSomePis 0 t              = return ([], t)
extractSomePis n (Pi a τ1 bτ2) = do
  let (b, τ2) = unscopeTerm bτ2
  (l, rest) <- extractSomePis (n - 1) τ2
  return ((a, b, τ1) : l, rest)
extractSomePis _ t              =
  let e :: String = printf "extractPis: not a Pi: %s" (prettyStrU t)
  in throwExc e

extractPis ::
  Member (Exc String) r =>
  TermX α Variable -> Eff r ([(α, Binder Variable, TermX α Variable)], TermX α Variable)
extractPis = \case
  Pi a τ1 bτ2 -> do
    let (b, τ2) = unscopeTerm bτ2
    (l, rest) <- extractPis τ2
    return ((a, b, τ1) : l, rest)
  t -> return ([], t)

extractPi ::
  Member (Exc String) r =>
  TermX α Variable -> Eff r (α, TermX α Variable, Binder Variable, TermX α Variable)
extractPi = \case
  Pi a τ1 bτ2 -> do
    let (b, τ2) = unscopeTerm bτ2
    return (a, τ1, b, τ2)
  t -> throwExc $ printf "extracePi: not a Pi: %s" (prettyStrU t)

mkLams :: [(α, Binder Variable)] -> TermX α Variable -> TermX α Variable
mkLams []          rest = rest
mkLams ((a, b) : t) rest = Lam a (abstractBinder b (mkLams t rest))

mkPis :: [(α, Binder Variable, TermX α Variable)] -> TermX α Variable -> TermX α Variable
mkPis []               rest = rest
mkPis ((a, b, τ1) : t) rest = Pi a τ1 (abstractBinder b (mkPis t rest))
