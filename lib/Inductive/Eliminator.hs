{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Inductive.Eliminator
  ( addRecursiveMotive
  , applyVariables
  , eliminatorName
  , mkCase
  , mkEliminatorRawType
  , mkEliminatorType
  , motive
  , unpackConstructors
  ) where

import           Data.String

import           Inductive.Inductive
import           Inductive.Motive
import           Term.Binder
import           Term.Term
import qualified Term.Raw as Raw
import           Utils

applyTerms :: α -> [TermX α Variable] -> TermX α Variable -> TermX α Variable
applyTerms α = foldlWith (mkApp α)

applyVariables :: α -> [(Variable, b)] -> TermX α Variable -> TermX α Variable
applyVariables α l = applyTerms α (map (Var Nothing . fst) l)

mkApp :: α -> TermX α ν -> TermX α ν -> TermX α ν
mkApp α a t = App α a t

quantifyVariables ::
  α -> [(Variable, TermX α Variable)] -> TermX α Variable -> TermX α Variable
quantifyVariables α l =
  quantifyBinders α (map (\ (v, τ) -> (Binder (Just v), τ)) l)

quantifyBinders ::
  α -> [(Binder Variable, TermX α Variable)] -> TermX α Variable ->
  TermX α Variable
quantifyBinders α = foldrWith (mkPi α)

mkPi ::
  α -> (Binder Variable, TypeX α Variable) -> TypeX α Variable -> TermX α Variable
mkPi α (b, τ1) τ2 = Pi α τ1 (abstractBinder b τ2)

-- `acc` will contain the concrete indices, and will be well-sorted since
-- we peel from the outermost application
unpackIfFullyAppliedInductive' ::
  Variable ->
  Φips α Variable ->
  Φiis α Variable ->
  TermX α Variable ->
  [TermX α Variable] -> Maybe [TermX α Variable]
unpackIfFullyAppliedInductive' n ips iis term acc = go term ips iis acc
  where
    go (Var _ v)   []        []        acc | v == n    = Just acc
                                           | otherwise = Nothing
    go _           []        []        _               = Nothing
    -- when ran out of indices, peel parameters
    go (App _ l _) (_ : ips) []        acc             = go l ips [] acc
    go (App _ l r) _         (_ : iis) acc             = go l ips iis (r : acc)
    go _           _         _         _               = Nothing

unpackIfFullyAppliedInductive ::
  Variable -> Φips α Variable -> Φiis α Variable -> TermX α Variable ->
  Maybe [TermX α Variable]
unpackIfFullyAppliedInductive n ips iis t =
  unpackIfFullyAppliedInductive' n ips iis t []

-- if the term is `inductiveName` fully-applied, replace it with
-- an instantiation of the motive
addRecursiveMotive ::
  α ->
  Variable ->
  Φips α Variable ->
  Φiis α Variable ->
  TermX α Variable ->
  (Variable, TypeX α Variable) ->
  [(Binder Variable, TypeX α Variable)]
addRecursiveMotive α n ips iis motive (v, τ) =
  case unpackIfFullyAppliedInductive n ips iis τ of
    Just indices ->
      [ (Binder (Just v), τ)
      , (Binder Nothing, App α (applyTerms α indices motive) (Var Nothing v))
      ]
    Nothing -> [(Binder (Just v), τ)]

mkCase ::
  α ->
  Variable -> Φips α Variable -> Φiis α Variable ->
  Variable -> Φcps α Variable -> Φcis α Variable ->
  TermX α Variable -> TermX α Variable
mkCase α n ips iis cn cps cis =
  -- quantify over constructor parameters, adding recursive hypotheses
  quantifyBinders α (concatMap (addRecursiveMotive α n ips iis motive) cps)
  . applyTerms α [applyVariables α cps (Var Nothing cn)]
  . applyTerms α cis

motive :: IsString a => a
motive = "Motive"

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
mkEliminatorType' :: ∀ α.
  α ->
  Variable ->
  [(Variable, TermX α Variable)] ->
  [(Variable, TermX α Variable)] ->
  [(Variable, [(Variable, TermX α Variable)], [TermX α Variable])] ->
  TypeX α Variable
mkEliminatorType'
  α inductiveName inductiveParameters inductiveIndices constructors =

  quantifyVariables   α inductiveParameters
  $ quantifyVariables α [(motive, motiveType)]
  $ quantifyCases
  $ quantifyVariables α inductiveIndices
  $ quantifyVariables α [(discriminee, discrimineeType)]
  $ outputType

  where

    discriminee :: IsString a => a
    discriminee = "instance"

    discrimineeType =
        applyVariables α inductiveIndices
      $ applyVariables α inductiveParameters
      $ Var Nothing inductiveName

    motiveType =
      mkMotiveType' α inductiveName inductiveParameters inductiveIndices Type

    outputType = App α (applyVariables α inductiveIndices motive) discriminee

    quantifyCases = foldrWith quantifyCase constructors

    quantifyCase (consName, consParameters, consIndices) acc =
      Pi α
      (mkCase α
       inductiveName inductiveParameters inductiveIndices
       consName consParameters consIndices
       motive)
      (abstractAnonymous acc)

unpackConstructor :: Constructor α ν -> (ν, Φcps α ν, Φcis α ν)
unpackConstructor (Constructor _ cn cps cis) = (cn, cps, cis)

unpackConstructors :: [Constructor α ν] -> [(ν, Φcps α ν, Φcis α ν)]
unpackConstructors = map unpackConstructor

mkEliminatorType :: α -> Inductive α Variable -> TypeX α Variable
mkEliminatorType α (Inductive n ps is cs) =
  mkEliminatorType' α n ps is (unpackConstructors cs)
  -- mkEliminatorType' α n ps is (instantiateConstructors cs)
  -- eliminatorType' α n ps (instantiateBinders "i" is) (instantiateConstructors cs)
  -- where
  --   instantiateConstructors = map instantiateConstructor
  --   instantiateConstructor (Constructor _ cn cps cis) = (cn, cps, cis)
  --     --(cn, instantiateBinders "p" cps, cis)

mkEliminatorRawType :: Inductive Raw.Raw Variable -> Raw.Type Variable
mkEliminatorRawType = mkEliminatorType ()

eliminatorName :: Variable -> Variable
eliminatorName (Variable v) = Variable (v ++ "_rect")
