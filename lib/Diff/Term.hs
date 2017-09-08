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

import           Control.Lens (_2, over)
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Text.Printf
import           Text.PrettyPrint.Annotated.WL

import qualified Diff.Atom as DA
import           Diff.Utils
import           PrettyPrinting.Term ()
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           Term.Binder
import           Term.Term

data Diff α
  = Same
  | Replace    (TermX α Variable)
  | CpyApp     (Diff α) (Diff α)
  | CpyLam     (DA.Diff (Binder Variable)) (Diff α)
  | CpyPi      (Diff α) (DA.Diff (Binder Variable)) (Diff α)
  | CpyVar     (DA.Diff Variable)
  | InsApp     α (Diff α) (Diff α)
  | InsLam     α (Binder Variable) (Diff α)
  | InsPi      α (Diff α) (Binder Variable) (Diff α)
  | PermutApps [Int] (Diff α)
  | PermutLams [Int] (Diff α)
  | PermutPis  [Int] (Diff α)
  deriving (Show)

instance PrettyPrintable (Diff α) where
  prettyDoc = \case
    Same              -> text "Same"
    Replace t         -> fillSep [ text "Replace",    go t ]
    CpyApp δ1 δ2      -> fillSep [ text "CpyApp",     go δ1, go δ2 ]
    CpyLam δ1 δ2      -> fillSep [ text "CpyLam",     go δ1, go δ2 ]
    CpyPi  δ1 δ2 δ3   -> fillSep [ text "CpyPi",      go δ1, go δ2, go δ3 ]
    CpyVar δ1         -> fillSep [ text "CpyVar",     go δ1 ]
    InsApp _ δ1 δ2    -> fillSep [ text "InsApp",     go δ1, go δ2 ]
    InsLam _ δ1 δ2    -> fillSep [ text "InsLam",     go δ1, go δ2 ]
    InsPi  _ δ1 δ2 δ3 -> fillSep [ text "InsPi",      go δ1, go δ2, go δ3 ]
    PermutApps p δ1   -> fillSep [ text "PermutApp",  (text $ show p), go δ1 ]
    PermutLams p δ1   -> fillSep [ text "PermutLam",  (text $ show p), go δ1 ]
    PermutPis  p δ1   -> fillSep [ text "PermutPi",   (text $ show p), go δ1 ]

    where
      go :: PrettyPrintable a => a -> Doc ()
      go = parens . prettyDoc

patch ::
  ( Member (Exc String) r
  , Member Trace r
  ) =>
  TermX α Variable -> Diff α -> Eff r (TermX α Variable)
patch t d =

  let exc (s :: String) = throwExc $ printf "Diff.Term/patch: %s" s in

  -- trace (printf "Diff.Term/patch:(%s, %s)" (preview t) (preview d)) >>

  case (t, d) of

  (_, Same) -> return t

  (_, Replace t') -> return t'

  (App a t1 t2, CpyApp d1 d2) -> App a <$> patch t1 d1 <*> patch t2 d2
  (_, CpyApp _ _) -> exc "CpyApp, not an App"

  (Lam a bt, CpyLam db dt) ->
    let (b, t') = unscopeTerm bt in
    Lam a <$> (abstractBinder <$> DA.patch b db <*> patch t' dt)
  (_, CpyLam _ _) -> exc "CpyLam, not a Lam"

  (Pi a τ1 bτ2, CpyPi d1 db d2) ->
    let (b, τ2) = unscopeTerm bτ2 in
    Pi a <$> patch τ1 d1 <*> (abstractBinder <$> DA.patch b db <*> patch τ2 d2)
  (_, CpyPi _ _ _) -> exc "CpyPi, not a Pi"

  (Var a v, CpyVar δv) -> Var a <$> DA.patch v δv
  (_, CpyVar _) -> exc $ printf "CpyVar, not a Var: %s" (prettyStr t)

  (_, InsApp a d1 d2) -> App a <$> patch t d1 <*> patch t d2

  (_, InsLam a b d1) -> Lam a . abstractBinder b <$> patch t d1

  (_, InsPi a d1 b d2) -> Pi a <$> patch t d1 <*> (abstractBinder b <$> patch t d2)

  (_, PermutApps p δ') -> do
    (fun, args) <- extractApps t
    patch (mkApps fun (permute p args)) δ'

  (_, PermutLams p d') -> do
    (lams, rest) <- extractSomeLams (length p) t
    patch (mkLams (permute p lams) rest) d'

  (_, PermutPis p d') -> do
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
  let e :: String = printf "extractLams: not a Lam: %s" (prettyStrU t) in
  throwExc e

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
  throwExc $ printf "extractPis: not a Pi: %s" (prettyStrU t)

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

mkApps :: TermX α Variable -> [(α, TermX α Variable)] -> TermX α Variable
mkApps f []           = f
mkApps f ((a, e) : t) = mkApps (App a f e) t

mkLams :: [(α, Binder Variable)] -> TermX α Variable -> TermX α Variable
mkLams []          rest = rest
mkLams ((a, b) : t) rest = Lam a (abstractBinder b (mkLams t rest))

mkPis :: [(α, Binder Variable, TermX α Variable)] -> TermX α Variable -> TermX α Variable
mkPis []               rest = rest
mkPis ((a, b, τ1) : t) rest = Pi a τ1 (abstractBinder b (mkPis t rest))
