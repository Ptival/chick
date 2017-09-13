module Inductive.Utils
  ( instantiateBinders
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
    instantiate (Binder Nothing,   τ) i = (Variable $ printf "%s%d" prefix i, τ)
