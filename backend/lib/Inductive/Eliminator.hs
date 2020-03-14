{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE OverloadedStrings #-}

module Inductive.Eliminator
  ( addRecursiveMotive
  , mkEliminatorName
  , mkCase
  , mkEliminatorRawType
  , mkEliminatorType
  , motive
  , unpackConstructors
  ) where

import           Data.Default        ( Default )
import           Data.Maybe          ( fromMaybe )
import           Data.String         ( IsString )

import           Inductive.Inductive
import           Inductive.Motive
import           Inductive.Utils
import           Term.Binder
import           Term.Term
import qualified Term.Raw            as Raw
import qualified Term.Universe       as U
import           Utils

-- `acc` will contain the concrete indices, and will be well-sorted since
-- we peel from the outermost application
unpackIfFullyAppliedInductive' ::
  Variable ->
  Φips α Variable ->
  Φiis α Variable ->
  TermX α Variable ->
  [(α, TermX α Variable)] -> Maybe [(α, TermX α Variable)]
unpackIfFullyAppliedInductive' n ips iis term = go term ips iis
  where
    go (Var _ v)   []        []        acc | v == n    = Just acc
                                           | otherwise = Nothing
    go _           []        []        _               = Nothing
    -- when ran out of indices, peel parameters
    go (App _ l _) (_ : ips) []        acc = go l ips [] acc
    go (App α l r) _         (_ : iis) acc = go l ips iis ((α, r) : acc)
    go _           _         _         _   = Nothing

unpackIfFullyAppliedInductive ::
  Variable -> Φips α Variable -> Φiis α Variable -> TermX α Variable ->
  Maybe [(α, TermX α Variable)]
unpackIfFullyAppliedInductive n ips iis t =
  unpackIfFullyAppliedInductive' n ips iis t []

{- if the term is `inductiveName` fully-applied, replace it with an
instantiation of the motive
-}
addRecursiveMotive ::
  Variable ->
  Φips α Variable ->
  Φiis α Variable ->
  TermX α Variable ->
  Φcp α Variable ->
  [(α, Binder Variable, TypeX α Variable)]
addRecursiveMotive n ips iis motive (α, b, τ) =
  let v = fromMaybe "__rec__" (unBinder b)
  in
  case unpackIfFullyAppliedInductive n ips iis τ of
    Just indices ->
      [ (α, Binder (Just v), τ)
      , (α, Binder Nothing, App α (applyTerms indices motive) (Var Nothing v))
      ]
    Nothing -> [(α, b, τ)]

mkCase ::
  Default α =>
  α ->
  Variable -> Φips α Variable -> Φiis α Variable ->
  Variable -> Φcps α Variable -> Φcis α Variable ->
  TermX α Variable -> TermX α Variable
mkCase α n ips iis cn cps cis =
  -- quantify over constructor parameters, adding recursive hypotheses
  quantifyBinders (concatMap (addRecursiveMotive n ips iis motive) cps)
  . applyTerms [(α, applyBinders cps (Var Nothing cn))]
  . applyTerms cis

-- can be used as a Variable or as a Var Term
motive :: IsString a => a
motive = "__motive__"

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
  Default α =>
  α ->
  Variable ->
  Φips α Variable ->
  Φiis α Variable ->
  U.Universe ->
  [(Variable, Φcps α Variable, Φcis α Variable)] ->
  TypeX α Variable
mkEliminatorType' α n ips iis _ cs =

  quantifyVariables   ips
  . quantifyVariables [(α, motive, motiveType)]
  . quantifyCases
  . quantifyBinders iis
  . quantifyVariables [(α, discriminee, discrimineeType)]
  $ outputType

  where

    discriminee :: IsString a => a
    discriminee = "__instance__"

    discrimineeType =
        applyBinders iis
      $ applyVariables ips
      $ Var Nothing n

    -- for now we only make the Type-motive, it's easy to make the Set and Prop ones too
    motiveType = mkMotiveType' α n ips iis (Type U.Type)

    outputType = App α (applyBinders iis motive) discriminee

    quantifyCases = foldrWith quantifyCase cs

    quantifyCase (consName, consParameters, consIndices) acc =
      Pi α
      (mkCase α n ips iis
       consName consParameters consIndices
       motive)
      (abstractAnonymous acc)

unpackConstructor :: Constructor α ν -> (ν, Φcps α ν, Φcis α ν)
unpackConstructor (Constructor _ cn cps cis) = (cn, cps, cis)

unpackConstructors :: [Constructor α ν] -> [(ν, Φcps α ν, Φcis α ν)]
unpackConstructors = map unpackConstructor

mkEliminatorType :: Default α => α -> Inductive α Variable -> TypeX α Variable
mkEliminatorType α (Inductive n ps is u cs) =
  mkEliminatorType' α n ps is u (unpackConstructors cs)
  -- mkEliminatorType' α n ps is (instantiateConstructors cs)
  -- eliminatorType' α n ps (instantiateBinders "i" is) (instantiateConstructors cs)
  -- where
  --   instantiateConstructors = map instantiateConstructor
  --   instantiateConstructor (Constructor _ cn cps cis) = (cn, cps, cis)
  --     --(cn, instantiateBinders "p" cps, cis)

mkEliminatorRawType :: Inductive Raw.Raw Variable -> Raw.Type Variable
mkEliminatorRawType = mkEliminatorType ()

mkEliminatorName :: Variable -> Variable
mkEliminatorName v = mkVariable (unVariable v ++ "_rect")
