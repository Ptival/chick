{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Inductive.Motive where

import Inductive.Inductive
import Term.Term
import Utils

-- TODO:
-- it's going to be a pervasive issue that things like unnamed indices will need to get
-- a consistent name at different places in the code
-- the functions should assume that names get picked and are unique
-- then callers should have a way to fully instantiate names in a given env/context

onInductiveIndexInside ::
  α -> TypeX α Variable -> Φii α Variable -> TermX α Variable
onInductiveIndexInside α t (v, _) = App α t (Var Nothing v)

onInductiveIndexOutside ::
  α -> Φii α Variable -> TypeX α Variable -> TermX α Variable
onInductiveIndexOutside α (v, i) t = Pi α i (abstractVariable v t)

onInductiveParameter ::
  α -> TypeX α Variable -> Φip α Variable -> TermX α Variable
onInductiveParameter α t (b, _) = App α t (Var Nothing b)

-- for instance, for Vec:
-- (n : nat) → Vec T n -> Type
mkMotiveType' :: ∀ α.
  α ->
  Variable ->
  Φips α Variable ->
  Φiis α Variable ->
  TypeX α Variable ->
  TypeX α Variable
mkMotiveType' α inductiveName inductiveParameters inductiveIndices universe =

  foldrWith (onInductiveIndexOutside α) inductiveIndices
  $ Pi α inductive (abstractAnonymous universe)

  where

    inductive :: TypeX α Variable
    inductive =
      foldlWith   (onInductiveIndexInside α) inductiveIndices
      $ foldlWith (onInductiveParameter   α) inductiveParameters
      $ Var Nothing inductiveName

mkMotiveType :: ∀ α.
  α ->
  Inductive α Variable ->
  TypeX α Variable ->
  TypeX α Variable
mkMotiveType α (Inductive n ps is _) universe =
  mkMotiveType' α n ps is universe
  -- mkMotiveType' α n ps (instantiateBinders "i" is) universe
