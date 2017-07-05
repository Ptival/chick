{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}

module Diff.Term
  ( Diff(..)
  , extractLams
  , extractPis
  , mkLams
  , mkPis
  , patch
  , permute
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Text.Printf

import qualified Diff.Atom as DA
import Diff.Utils
import           PrettyPrinting.PrettyPrintableUnannotated
import           Term.Binder
import           Term.Term
import qualified Term.Raw as Raw
import           Term.Variable

data Diff α
  = Same
  | Change (TermX α Variable)
  | CpyLam (DA.Diff (Binder Variable)) (Diff α)
  | InsLam (Binder Variable) (Diff α)
  | PermutLams [Int] (Diff α)
  | CpyPi (Diff α) (DA.Diff (Binder Variable)) (Diff α)
  | InsPi (Diff α) (Binder Variable) (Diff α)
  | PermutPis [Int] (Diff α)
  deriving (Show)

patch ::
  Member (Exc String) r =>
  Raw.Term Variable -> Diff Raw.Raw -> Eff r (Raw.Term Variable)
patch t d =
  case d of
    Same          -> return t
    Change t'     -> return t'
    CpyLam db dt  ->
      case t of
        Lam a bt ->
          let (b, t') = unscopeTerm bt in
          Lam a <$> (abstractBinder <$> DA.patch b db <*> patch t' dt)
        _           -> throwExc "patch: CpyLam, not a Lam"
    InsLam  b d1  -> Lam () . abstractBinder b <$> patch t d1
    PermutLams p d' -> do
      (lams, rest) <- extractLams (length p) t
      patch (mkLams (permute p lams) rest) d'
    CpyPi d1 db d2 ->
      case t of
        Pi a τ1 bτ2 ->
          let (b, τ2) = unscopeTerm bτ2 in
          Pi a <$> patch τ1 d1 <*> (abstractBinder <$> DA.patch b db <*> patch τ2 d2)
        _           -> throwExc "patch: CpyPi, not a Pi"
    InsPi d1 b d2 -> Pi () <$> patch t d1 <*> (abstractBinder b <$> patch t d2)
    PermutPis p d' -> do
      (pis, rest) <- extractPis (length p) t
      patch (mkPis (permute p pis) rest) d'

extractLams ::
  Member (Exc String) r =>
  Int -> Raw.Term Variable -> Eff r ([Binder Variable], Raw.Term Variable)
extractLams 0 t           = return ([], t)
extractLams n (Lam () bt) = do
  let (b, t) = unscopeTerm bt
  (l, rest) <- extractLams (n - 1) t
  return (b : l, rest)
extractLams _ t              =
  let e :: String = printf "extractLams: not a Lam: %s" (prettyStrU t)
  in throwExc e

extractPis ::
  Member (Exc String) r =>
  Int -> Raw.Term Variable -> Eff r ([(Binder Variable, Raw.Term Variable)], Raw.Term Variable)
extractPis 0 t              = return ([], t)
extractPis n (Pi () τ1 bτ2) = do
  let (b, τ2) = unscopeTerm bτ2
  (l, rest) <- extractPis (n - 1) τ2
  return ((b, τ1) : l, rest)
extractPis _ t              =
  let e :: String = printf "extractPis: not a Pi: %s" (prettyStrU t)
  in throwExc e

mkLams :: [Binder Variable] -> Raw.Term Variable -> Raw.Term Variable
mkLams []      rest = rest
mkLams (b : t) rest = Lam () (abstractBinder b (mkLams t rest))

mkPis :: [(Binder Variable, Raw.Term Variable)] -> Raw.Term Variable -> Raw.Term Variable
mkPis []            rest = rest
mkPis ((b, τ1) : t) rest = Pi () τ1 (abstractBinder b (mkPis t rest))

-- | Dumbest way to implement this
permute :: [Int] -> [a] -> [a]
permute []      _ = []
permute (h : t) l = (l !! h) : permute t l
