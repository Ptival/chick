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
  ( Constructor(..)
  , Inductive(..)
  , constructorCheckedType
  , constructorCheckedType'
  , constructorRawType
  , constructorRawType'
  , inductiveRawType
  , inductiveRawType'
  , inductiveType
  , inductiveType'
  , mkMotiveType
  , eliminatorRawType
  ) where

import           Control.Lens (_2, over)
import           Control.Monad.Reader
import           Data.Default
import           Data.String

-- import           Inductive.Constructor
import           Precedence
import           PrettyPrinting.Term ()
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Term
import qualified Term.TypeChecked as C
import           Term.Variable
import           Text.PrettyPrint.Annotated.WL
import           Text.Printf

data Inductive α ν =
  Inductive
  { inductiveName         :: ν
  , inductiveParameters   :: [(ν, TypeX α ν)]
  , inductiveIndices      :: [(Binder ν, TypeX α ν)]
  , inductiveConstructors :: [Constructor α ν]
  }

deriving instance (Show α, Show ν) => Show (Inductive α ν)

-- Deriving Eq does not do what I want, because it does not equate two inductives
-- when they differ over an unused binder name.  I'd rather use α-equivalence
-- of all the things involved
instance Eq (Inductive α Variable) where
  indA == indB =
    inductiveRawType (rawInductive indA) == inductiveRawType (rawInductive indB)

data Constructor α ν =
  Constructor
  { constructorInductive  :: Inductive α ν
  , constructorName       :: ν
  , constructorParameters :: [(Binder ν, TypeX α ν)]
  , constructorIndices    :: [TypeX α ν]
  }

-- /!\ DO NOT DERIVE ANY TYPECLASS FOR `Constructor` AS IT IS CYCLIC WITH `Inductive` /!\

instance Eq (Constructor α Variable) where
  (Constructor _ n ps is) == (Constructor _ n' ps' is') =
    (n, ps, is) == (n', ps', is')

instance (Show α, Show ν) => Show (Constructor α ν) where
  show (Constructor _ n ps is) = printf "Constructor _ %s %s %s" (show n) (show ps) (show is)

mapRawSnd :: [(a, TermX α ν)] -> [(a, Raw.Term ν)]
mapRawSnd = map (over _2 Raw.raw)

rawInductive :: Inductive α ν -> Inductive Raw.Raw ν
rawInductive (Inductive n ps is cs) =
  fix $ \ ind' ->
          Inductive n (mapRawSnd ps) (mapRawSnd is) (map (rawConstructor ind') cs)

rawConstructor :: Inductive Raw.Raw ν -> Constructor α ν -> Constructor Raw.Raw ν
rawConstructor rawInd (Constructor _ n ps is) =
  Constructor rawInd n (mapRawSnd ps) (map Raw.raw is)

constructorType' :: ∀ α.
  α ->
  Variable ->
  [(Variable, TermX α Variable)] ->
  [(Binder Variable, TermX α Variable)] ->
  [TermX α Variable] ->
  TypeX α Variable
constructorType' α indName indParams consParams consIndices =
  foldr onParam (
    foldr onIndex (
        foldr onIndParam
        (Var Nothing indName)
        indParams)
    consIndices)
  consParams
  where
    onIndex :: TermX α Variable -> TermX α Variable -> TermX α Variable
    onIndex i t = App α t i --(Raw.raw t) (Raw.raw i)
    onParam :: (Binder Variable, TermX α Variable) -> TermX α Variable -> TermX α Variable
    onParam (b, p) t = Pi α p (abstractBinder b t)
    onIndParam :: (Variable, TermX α Variable) -> TermX α Variable -> TermX α Variable
    onIndParam (v, _) t = App α t (Var Nothing v)

constructorRawType' ::
  Variable ->
  [(Variable, TermX α Variable)] ->
  [(Binder Variable, TermX α Variable)] ->
  [TermX α Variable] ->
  Raw.Type Variable
constructorRawType' indName indParams consParams consIndices =
  constructorType' () indName indParams' consParams' consIndices'
  where
    indParams'   = map (over _2 Raw.raw) indParams
    consParams'  = map (over _2 Raw.raw) consParams
    consIndices' = map Raw.raw consIndices

constructorRawType :: Constructor Raw.Raw Variable -> Raw.Type Variable
constructorRawType (Constructor (Inductive indName indParams _ _) _ consParams consIndices) =
  constructorRawType' indName indParams consParams consIndices

-- | Constructs the type of the inductive type
inductiveRawType' ::
  [(Variable, Raw.Type Variable)] ->
  [(Binder Variable, Raw.Type Variable)] ->
  Raw.Type Variable
inductiveRawType' indParams indIndices =
  foldr onParam (foldr onIndex Type indIndices) indParams
  where
    -- onIndexOrParam :: (Binder Variable, Raw.Type Variable) -> Raw.Type Variable -> Raw.Type Variable
    onParam (v, p) t = Pi () p (abstractVariable v t)
    onIndex (b, i) t = Pi () i (abstractBinder   b t)

-- | Constructs the type of the inductive type
inductiveRawType :: Inductive Raw.Raw Variable -> Raw.Type Variable
inductiveRawType (Inductive _ ps is _) = inductiveRawType' ps is

constructorCheckedType' ::
  Variable ->
  [(Variable, C.Type Variable)] ->
  [(Binder Variable, C.Type Variable)] ->
  [C.Type Variable] ->
  C.Type Variable
constructorCheckedType' = constructorType' (C.Checked Type)

constructorCheckedType :: Constructor (C.Checked Variable) Variable -> C.Type Variable
constructorCheckedType (Constructor (Inductive indName indParams _ _) _ consParams consIndices) =
  constructorCheckedType' indName indParams consParams consIndices

-- | Sometimes, we can't call `inductiveType` because the constructors are not checked yet.
-- | `inductiveType'` lets us call with just the minimal information.
inductiveType' ::
  [(Variable, C.Type Variable)] ->
  [(Binder Variable, C.Type Variable)] ->
  C.Type Variable
inductiveType' ps is =
  foldr onParam (foldr onIndex Type is) ps
  where
    -- onIndexOrParam :: C.Type Variable -> C.Type Variable -> C.Type Variable
    onParam (v, p) t = Pi (C.Checked Type) p (abstractVariable v t)
    onIndex (b, i) t = Pi (C.Checked Type) i (abstractBinder   b t)

inductiveType :: Inductive (C.Checked Variable) Variable -> C.Type Variable
inductiveType (Inductive _ ps is _) = inductiveType' ps is

arrows :: [Doc a] -> Doc a
arrows = encloseSep mempty mempty (text " →")

boundTermDocBinder ::
  MonadReader PrecedenceTable m =>
  (Binder Variable, TermX α Variable) -> m (Doc ())
boundTermDocBinder (Binder b, t) =
  case b of
    Nothing -> prettyDocU t
    Just v -> boundTermDocVariable (v, t)

boundTermDocVariable ::
  MonadReader PrecedenceTable m =>
  (Variable, TermX α Variable) -> m (Doc ())
boundTermDocVariable (v, t) = do
  tDoc <- prettyDocU t
  return $ parens . fillSep $ [ prettyDoc v, text ":", tDoc ]

instance PrettyPrintableUnannotated (Inductive α Variable) where
  prettyDocU (Inductive n ps is cs) = do
    psDoc <- mapM boundTermDocVariable ps
    csDoc <- mapM prettyDocU cs
    isDoc <- mapM boundTermDocBinder is
    return $ vsep $
      [ fillSep $
        [ text "Inductive"
        , prettyDoc n
        ]
        ++
        (
          -- mempty creates an unwanted space, so have to use []
          if length ps == 0
          then []
          else [encloseSep mempty mempty mempty psDoc]
        )
        ++
        [ text ":"
        , arrows (isDoc ++ [text "Type"])
        , text ":="
        ]
      ]
      ++ (map (\ x -> fillSep [ text "|", x]) csDoc)
      ++ [ text "." ]

instance PrettyPrintableUnannotated (Constructor α Variable) where
  prettyDocU (Constructor (Inductive indName indParams _ _) cName cParams cIndices) = do
    cDoc <- prettyDocU (constructorRawType' indName indParams cParams cIndices)
    return $ fillSep
      [ prettyDoc cName
      , text ":"
      , cDoc
      ]

instance PrettyPrintable (Constructor α Variable) where
  prettyDoc c = runReader (prettyDocU c) def

instance PrettyPrintable (Inductive α Variable) where
  prettyDoc i = runReader (prettyDocU i) def

-- TODO:
-- it's going to be a pervasive issue that things like unnamed indices will need to get
-- a consistent name at different places in the code
-- the functions should assume that names get picked and are unique
-- then callers should have a way to fully instantiate names in a given env/context

-- for instance, for Vec:
-- (n : nat) → Vec T n -> Type
mkMotiveType' :: ∀ α.
  α ->
  Variable ->
  [(Variable, TermX α Variable)] ->
  [(Variable, TermX α Variable)] ->
  TypeX α Variable ->
  TypeX α Variable
mkMotiveType' α indName indParams indIndices universe =
  foldr onIndIndexOutside (Pi α inductive (abstractAnonymous universe)) indIndices
  where
    inductive :: TypeX α Variable
    inductive = foldl onIndIndexInside (foldl onIndParam (Var Nothing indName) indParams) indIndices
    onIndIndexOutside (v, p) t = Pi α p (abstractVariable v t)
    onIndParam :: TypeX α Variable -> (Variable, TermX α Variable) -> TermX α Variable
    onIndParam  t (b, _) = App α t (Var Nothing b)
    onIndIndexInside :: TypeX α Variable -> (Variable, TermX α Variable) -> TermX α Variable
    onIndIndexInside t (v, _) = App α t (Var Nothing v)

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f l = fst $ foldr (\ a (acc, i) -> (f a i : acc, i + 1)) ([], 0) l

instantiateBinders ::
  String ->
  [(Binder Variable, TypeX α Variable)] ->
  [(Variable, TypeX α Variable)]
instantiateBinders prefix = mapWithIndex instantiate
  where
    instantiate (Binder (Just v ), τ) _ = (v,      τ)
    instantiate (Binder Nothing,   τ) i = (Variable $ printf "%s%d" prefix i, τ)

mkMotiveType :: ∀ α.
  α ->
  Inductive α Variable ->
  TypeX α Variable ->
  TypeX α Variable
mkMotiveType α (Inductive n ps is _) universe =
  mkMotiveType' α n ps (instantiateBinders "i" is) universe

-- forall (A : Type) (P : forall n : nat, Vec A n -> Set),
--   P 0 (vnil A) ->
--   (forall (h : A) (n : nat) (t : t A n), P n t -> P (S n) (vcons A h n t)) ->
--   forall (n : nat) (t : Vec A n), P n t

-- The structure can be summarized as:
-- 1. quantify over the inductive parameters p1 p2
-- 2. quantify over the output property P
-- 3. for each constructor:
--   - quantify over all parameters cp1 cp2, but whenever the parameter is recursive,
--     add an appropriate P
--   - return (P ci1 ci2 (Constructor cp1 cp2))
-- 4. quantify over indices i1 i2
-- 5. quantify over one instance t of the input type (T ip1 ip2 i1 i2)
-- 6. return P i1 i2 t
eliminatorType' :: ∀ α.
  α ->
  Variable ->
  [(Variable, TermX α Variable)] ->
  [(Variable, TermX α Variable)] ->
  [(Variable, [(Variable, TermX α Variable)], [TermX α Variable])] ->
  TypeX α Variable
eliminatorType' α inductiveName inductiveParameters inductiveIndices constructors =

    quantifyVariables inductiveParameters
  $ quantifyVariables [(motive, motiveType)]
  $ quantifyCases
  $ quantifyVariables inductiveIndices
  $ quantifyVariables [(discriminee, discrimineeType)]
  $ outputType

  where

    -- apply :: [(Variable, TermX α Variable)] -> TermX α Variable -> TermX α Variable
    applyTerm = foldlWith mkApp
    applyVar l = applyTerm (map (Var Nothing . fst) l)
    foldlWith f l a = foldl f a l
    mkApp a t = App α a t

    -- quantifyVariables ::
    -- [(Variable, TermX α Variable)] -> TermX α Variable -> TermX α Variable
    quantifyVariables l = quantifyBinders (map (\ (v, τ) -> (Binder (Just v), τ)) l)
    -- quantifyBinders ::
    -- [(Binder Variable, TermX α Variable)] -> TermX α Variable -> TermX α Variable
    quantifyBinders = foldrWith mkPi
    foldrWith f l a = foldr f a l
    mkPi (b, τ) a = Pi α τ (abstractBinder b a)

    discriminee :: IsString a => a
    discriminee = "instance"

    discrimineeType =
        applyVar inductiveIndices
      $ applyVar inductiveParameters
      $ Var Nothing inductiveName

    motive :: IsString a => a
    motive = "Motive"

    motiveType =
      mkMotiveType' α inductiveName inductiveParameters inductiveIndices Type

    outputType = App α (applyVar inductiveIndices motive) discriminee

    quantifyCases = foldrWith quantifyCase constructors

    quantifyCase (consName, consParameters, consIndices) acc =
      Pi α (mkCase consName consParameters consIndices) (abstractAnonymous acc)

    mkCase consName consParameters consIndices =
        quantifyBinders (concatMap addRecursiveMotive consParameters)
      $ applyTerm [applyVar consParameters (Var Nothing consName)]
      $ applyTerm consIndices
      $ motive

    -- if the term is `inductiveName` fully-applied, replace it with
    -- an instantiation of the motive
    addRecursiveMotive ::
      (Variable, TypeX α Variable) -> [(Binder Variable, TypeX α Variable)]
    addRecursiveMotive (v, τ) =
      case unpackFullyAppliedInductive τ of
        Just indices ->
          [ (Binder (Just v), τ)
          , (Binder Nothing, App α (applyTerm indices motive) (Var Nothing v))
          ]
        Nothing -> [(Binder (Just v), τ)]

    -- `acc` will contain the concrete indices, and will be well-sorted since
    -- we peel from the outermost application
    unpackFullyAppliedInductive' term nbParams nbIndices acc =
      case (term, nbParams, nbIndices) of
        (Var _ v, 0, 0) | v == inductiveName -> Just acc
                        | otherwise          -> Nothing
        (_, 0, 0) -> Nothing
        -- when ran out of indices, peel parameters
        (App _ l _, _, 0) ->
          unpackFullyAppliedInductive' l (nbParams - 1) nbIndices       acc
        (App _ l r, _, _) ->
          unpackFullyAppliedInductive' l nbParams       (nbIndices - 1) (r : acc)
        (_, _, _) -> Nothing

    unpackFullyAppliedInductive t =
      unpackFullyAppliedInductive' t
      (length inductiveParameters) (length inductiveIndices) []

eliminatorRawType :: Inductive Raw.Raw Variable -> Raw.Type Variable
eliminatorRawType (Inductive n ps is cs) =
  eliminatorType' () n ps (instantiateBinders "i" is) (instantiateConstructors cs)
  where
    instantiateConstructors = map instantiateConstructor
    instantiateConstructor (Constructor _ cn cps cis) =
      (cn, instantiateBinders "p" cps, cis)
