{-# LANGUAGE OverloadedStrings #-}

module Inductive.Utils
  ( applyBinders
  , applyTerms
  , applyVariables
  , instantiateBinders
  , quantifyBinders
  , quantifyVariables
  ) where

import Text.Printf

import Term.Term
import Utils

instantiateBinders ::
  String ->
  [(Binder Variable, TypeX α Variable)] ->
  [(Variable, TypeX α Variable)]
instantiateBinders prefix = mapWithIndex instantiate
  where
    instantiate (Binder (Just v ), τ) _ = (v,      τ)
    instantiate (Binder Nothing,   τ) i = (mkVariable $ printf "%s%d" prefix i, τ)

applyTerms :: [(α, TermX α Variable)] -> TermX α Variable -> TermX α Variable
applyTerms = foldlWith (\ a (α, t) -> mkApp α a t)

applyVariables :: [(α, Variable, b)] -> TermX α Variable -> TermX α Variable
applyVariables l = applyTerms (map (\ (α, ν, _) -> (α, Var Nothing ν)) l)

applyBinders :: [(α, Binder Variable, b)] -> TermX α Variable -> TermX α Variable
applyBinders l = applyTerms (map (\ (α, b, _) -> (α, varForBinder b)) l)
  where
    varForBinder b = case unBinder b of
      Nothing -> Hole (error "applyBinders")
      Just v  -> Var Nothing v

mkApp :: α -> TermX α ν -> TermX α ν -> TermX α ν
mkApp α a t = App α a t

quantifyVariables ::
  [(α, Variable, TermX α Variable)] -> TermX α Variable -> TermX α Variable
quantifyVariables =
  quantifyBinders . map (\ (α, v, τ) -> (α, Binder (Just v), τ))

quantifyBinders ::
  [(α, Binder Variable, TermX α Variable)] -> TermX α Variable -> TermX α Variable
quantifyBinders = foldrWith (\ (α, b, τ) a -> mkPi α (b, τ) a)

mkPi ::
  α -> (Binder Variable, TypeX α Variable) -> TypeX α Variable -> TermX α Variable
mkPi α (b, τ1) τ2 = Pi α τ1 (abstractBinder b τ2)
