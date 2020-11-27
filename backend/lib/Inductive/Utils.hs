{-# LANGUAGE OverloadedStrings #-}

-- | This module contains utility functions that help in creating datatypes
-- relating to inductive constructions.
module Inductive.Utils
  ( applyBinders,
    applyTerms,
    applyVariables,
    instantiateBinders,
    quantifyBinders,
    quantifyVariables,
  )
where

import Data.Default (Default (..))
import Term.Term
  ( Binder (..),
    TermX (App, Hole, Pi, Var),
    TypeX,
    Variable,
    abstractBinder,
    mkVariable,
  )
import Text.Printf (printf)
import Utils (foldlWith, foldrWith, mapWithIndex)

instantiateBinders ::
  String ->
  [(Binder Variable, TypeX α Variable)] ->
  [(Variable, TypeX α Variable)]
instantiateBinders prefix = mapWithIndex instantiate
  where
    instantiate (Binder (Just v), τ) _ = (v, τ)
    instantiate (Binder Nothing, τ) i = (mkVariable $ printf "%s%d" prefix i, τ)

-- | N-ary function application. The first argument is a list of arguments for
-- the n-ary function application, paired with the annotation for the
-- application node. The second argument is the function that receives these
-- arguments.
applyTerms :: [(α, TermX α Variable)] -> TermX α Variable -> TermX α Variable
applyTerms = foldlWith (\a (α, t) -> mkApp α a t)

-- | Special case of 'applyTerms' where we apply variables only.
applyVariables :: [(α, Variable)] -> TermX α Variable -> TermX α Variable
applyVariables l = applyTerms (map (\(α, ν) -> (α, Var Nothing ν)) l)

applyBinders ::
  Default α =>
  [(α, Binder Variable, b)] ->
  TermX α Variable ->
  TermX α Variable
applyBinders l = applyTerms (map (\(α, b, _) -> (α, varForBinder b)) l)
  where
    varForBinder b = case unBinder b of
      Nothing -> Hole def
      Just v -> Var Nothing v

mkApp :: α -> TermX α ν -> TermX α ν -> TermX α ν
mkApp α a t = App α a t

quantifyVariables ::
  [(α, Variable, TermX α Variable)] -> TermX α Variable -> TermX α Variable
quantifyVariables =
  quantifyBinders . map (\(α, v, τ) -> (α, Binder (Just v), τ))

quantifyBinders ::
  [(α, Binder Variable, TermX α Variable)] -> TermX α Variable -> TermX α Variable
quantifyBinders = foldrWith (\(α, b, τ) a -> mkPi α (b, τ) a)

mkPi ::
  α -> (Binder Variable, TypeX α Variable) -> TypeX α Variable -> TermX α Variable
mkPi α (b, τ1) τ2 = Pi α τ1 (abstractBinder b τ2)
