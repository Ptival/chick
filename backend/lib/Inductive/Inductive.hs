{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Inductive.Inductive
  ( Φip
  , Φips
  , Φii
  , Φiis
  , Φcp
  , Φcps
  , Φci
  , Φcis
  , Constructor(..)
  , Inductive(..)
  , applyConstructorIndices
  , constructorCheckedType
  , constructorCheckedType'
  , constructorRawType
  , constructorRawType'
  , cpAnnotation
  , cpBinder
  , cpType
  , inductiveRawType
  , inductiveRawType'
  , inductiveType
  , inductiveType'
  , ipAnnotation
  , ipBinder
  , ipType
  , quantifyConstructorParameters
  , quantifyInductiveIndices
  , quantifyInductiveParameters
  , rawInductive
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Default

import           Inductive.Utils
import           Precedence
import           PrettyPrinting.Term ()
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Term
import qualified Term.TypeChecked as C
import           Term.Universe (Universe)
import           Text.PrettyPrint.Annotated.WL
import           Text.Printf
import           Utils

type Φip  α ν = (α, ν, TypeX α ν)
type Φips α ν = [Φip α ν]

ipAnnotation :: Lens' (Φip  α ν) α
ipAnnotation = _1
ipBinder     :: Lens' (Φip  α ν) ν
ipBinder     = _2
ipType       :: Lens' (Φip  α ν) (TypeX α ν)
ipType       = _3

type Φii  α ν = (α, Binder ν, TypeX α ν)
type Φiis α ν = [Φii α ν]

data Inductive α ν =
  Inductive
  { inductiveName         :: ν
  , inductiveParameters   :: Φips α ν
  , inductiveIndices      :: Φiis α ν
  , inductiveUniverse     :: Universe
  , inductiveConstructors :: [Constructor α ν]
  }

deriving instance (Show α, Show ν) => Show (Inductive α ν)

-- need to write instance manually to avoid comparing annotations
instance Eq (Inductive α Variable) where
  Inductive n1 ps1 is1 u1 cs1 == Inductive n2 ps2 is2 u2 cs2 =
    let dropAnnotParams  (_, a, b) = (a, b) in
    let dropAnnotIndices (_, a, b) = (a, b) in
    n1 == n2
    && map dropAnnotParams ps1 == map dropAnnotParams ps2
    && map dropAnnotIndices is1 == map dropAnnotIndices is2
    && u1 == u2
    && cs1 == cs2

-- Deriving Eq does not do what I want, because it does not equate two inductives
-- when they differ over an unused binder name.  I'd rather use α-equivalence
-- of all the things involved
-- instance Eq (Inductive α Variable) where

type Φcp  α ν = (α, Binder ν, TypeX α ν)
type Φcps α ν = [Φcp α ν]

cpAnnotation :: Lens' (Φcp  α ν) α
cpAnnotation = _1
cpBinder     :: Lens' (Φcp  α ν) (Binder ν)
cpBinder     = _2
cpType       :: Lens' (Φcp  α ν) (TypeX α ν)
cpType       = _3

type Φci  α ν = (α, TypeX α ν)
type Φcis α ν = [Φci α ν]

data Constructor α ν =
  Constructor
  { constructorInductive  :: Inductive α ν
  , constructorName       :: ν
  , constructorParameters :: Φcps α ν
  , constructorIndices    :: Φcis α ν
  }

-- /!\ DO NOT DERIVE ANY TYPECLASS FOR `Constructor` AS IT IS CYCLIC WITH `Inductive` /!\

instance Eq (Constructor α Variable) where
  (Constructor _ n ps is) == (Constructor _ n' ps' is') =
    (n, map cpRaw ps, map ciRaw is) == (n', map cpRaw ps', map ciRaw is')

instance (Show α, Show ν) => Show (Constructor α ν) where
  show (Constructor _ n ps is) =
    printf "Constructor _ %s %s %s" (show n) (show ps) (show is)

ipRaw :: Φip α ν -> Φip Raw.Raw ν
ipRaw (_, ν, τ) = ((), ν, Raw.raw τ)

iiRaw :: Φii α ν -> Φii Raw.Raw ν
iiRaw (_, ν, τ) = ((), ν, Raw.raw τ)

cpRaw :: Φcp α ν -> Φcp Raw.Raw ν
cpRaw (_, ν, τ) = ((), ν, Raw.raw τ)

ciRaw :: Φci α ν -> Φci Raw.Raw ν
ciRaw (_, τ) = ((), Raw.raw τ)

rawInductive :: Inductive α ν -> Inductive Raw.Raw ν
rawInductive (Inductive n ips iis u ics) =
  fix $ \ ind' ->
  Inductive n (map ipRaw ips) (map iiRaw iis) u (map (rawConstructor ind') ics)

rawConstructor :: Inductive Raw.Raw ν -> Constructor α ν -> Constructor Raw.Raw ν
rawConstructor rawInd (Constructor _ cn cps cis) =
  Constructor rawInd cn (map cpRaw cps) (map ciRaw cis)

quantifyConstructorParameters ::
  Φcps α Variable -> TypeX α Variable -> TypeX α Variable
quantifyConstructorParameters = foldrWith onParam
  where
    onParam (α, b, p) t = Pi α p (abstractBinder b t)

applyConstructorIndices ::
  Φcis α Variable -> TypeX α Variable -> TypeX α Variable
applyConstructorIndices = foldrWith onIndex
  where
    onIndex (α, i) t = App α t i

constructorType' :: ∀ α.
  Bool ->
  Variable -> Φips α Variable -> Φcps α Variable -> Φcis α Variable ->
  TypeX α Variable
constructorType' shouldQuantifyIndParams n ips cps cis =
  (if shouldQuantifyIndParams
   then quantifyInductiveParameters ips
   else id
  )
  $ quantifyConstructorParameters cps
  $ applyConstructorIndices       cis
  $ applyVariables                ips
  $ Var Nothing n

constructorRawType' ::
  Bool -> Variable -> Φips α Variable -> Φcps α Variable -> Φcis α Variable ->
  Raw.Type Variable
constructorRawType' b n ips cps cis =
  constructorType' b n ips' cps' cis'
  where
    ips' = map ipRaw ips
    cps' = map cpRaw cps
    cis' = map ciRaw cis

{- `Bool`: whether inductive parameters should be quantified -}
constructorRawType :: Bool -> Constructor Raw.Raw Variable -> Raw.Type Variable
constructorRawType b (Constructor (Inductive n ips _ _ _) _ cps cis) =
  constructorRawType' b n ips cps cis

quantifyInductiveParameters ::
  Φips α Variable -> TypeX α Variable -> TypeX α Variable
quantifyInductiveParameters = foldrWith onParam
  where
    onParam (α, v, p) t = Pi α p (abstractVariable v t)

quantifyInductiveIndices ::
  Φiis α Variable -> TypeX α Variable -> TypeX α Variable
quantifyInductiveIndices = foldrWith onIndex
  where
    onIndex (α, b, i) t = Pi α i (abstractBinder b t)

-- | Constructs the type of the inductive type
inductiveRawType' ::
  Universe ->
  Φips α Variable ->
  Φiis α Variable ->
  Raw.Type Variable
inductiveRawType' univ ips iis =
  Raw.raw
  $ quantifyInductiveParameters ips
  $ quantifyInductiveIndices    iis
  $ Type univ

-- | Constructs the type of the inductive type
inductiveRawType :: Inductive α Variable -> Raw.Type Variable
inductiveRawType (Inductive _ ips iis u _) = inductiveRawType' u ips iis

constructorCheckedType' ::
  Bool ->
  Variable ->
  Φips (C.Checked Variable) Variable ->
  Φcps (C.Checked Variable) Variable ->
  Φcis (C.Checked Variable) Variable ->
  C.Type Variable
constructorCheckedType' = constructorType'

constructorCheckedType ::
  Bool -> Constructor (C.Checked Variable) Variable -> C.Type Variable
constructorCheckedType b (Constructor (Inductive n ips _ _ _) _ cps cis) =
  constructorCheckedType' b n ips cps cis

inductiveFamilyType' :: Φiis α Variable -> Universe -> TypeX α Variable
inductiveFamilyType' iis u = foldrWith onIndex iis (Type u)
  where onIndex (α, b, i) t = Pi α i (abstractBinder b t)

-- | Sometimes, we can't call `inductiveType` because the constructors are not
-- | checked yet.
-- | `inductiveType'` lets us call with just the minimal information.
inductiveType' ::
  Φips α Variable -> Φiis α Variable -> Universe -> TypeX α Variable
inductiveType' ips iis u = foldrWith onParam ips $ inductiveFamilyType' iis u
  where onParam (α, v, p) t = Pi α p (abstractVariable v t)

inductiveType :: Inductive (C.Checked Variable) Variable -> C.Type Variable
inductiveType (Inductive _ ps is u _) = inductiveType' ps is u

-- arrows :: [Doc a] -> Doc a
-- arrows = encloseSep mempty mempty (text " →")

_boundTermDocBinder ::
  MonadReader PrecedenceTable m =>
  (α, Binder Variable, TermX α Variable) -> m (Doc ())
_boundTermDocBinder (α, Binder b, t) =
  case b of
    Nothing -> prettyDocU t
    Just v -> boundTermDocVariable (α, v, t)

boundTermDocVariable ::
  MonadReader PrecedenceTable m =>
  (α, Variable, TermX α Variable) -> m (Doc ())
boundTermDocVariable (_, v, t) = do
  tDoc <- prettyDocU t
  return $ parens . fillSep $ [ prettyDoc v, text ":", tDoc ]

instance PrettyPrintableUnannotated (Inductive α Variable) where
  prettyDocU (Inductive n ips iis u cs) = do
    psDoc <- mapM boundTermDocVariable ips
    csDoc <- mapM prettyDocU cs
    isDoc <- prettyDocU (inductiveFamilyType' iis u)
    -- isDoc <- mapM boundTermDocBinder is
    -- isDoc <- mapM boundTermDocVariable is
    return $ vsep $
      [ fillSep $
        [ text "Inductive"
        , prettyDoc n
        ]
        ++
        (
          -- mempty creates an unwanted space, so have to use []
          if length ips == 0
          then []
          else [encloseSep mempty mempty mempty psDoc]
        )
        ++
        [ text ":"
        -- , arrows (isDoc ++ [text "Type"])
        , isDoc
        , text ":="
        ]
      ]
      ++ (map (\ x -> fillSep [ text "|", x]) csDoc)

instance PrettyPrintableUnannotated (Constructor α Variable) where
  prettyDocU (Constructor (Inductive n ips _ _ _) cName cParams cIndices) = do
    cDoc <- prettyDocU (constructorRawType' False n ips cParams cIndices)
    return $ fillSep
      [ prettyDoc cName
      , text ":"
      , cDoc
      ]

instance PrettyPrintable (Constructor α Variable) where
  prettyDoc c = runReader (prettyDocU c) def

instance PrettyPrintable (Inductive α Variable) where
  prettyDoc i = runReader (prettyDocU i) def
