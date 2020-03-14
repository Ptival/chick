{-# OPTIONS_GHC -fno-warn-name-shadowing #-}


module Inductive.Motive where

import Inductive.Inductive
import Term.Term
import Utils

-- TODO:
-- it's going to be a pervasive issue that things like unnamed indices will need to get
-- a consistent name at different places in the code
-- the functions should assume that names get picked and are unique
-- then callers should have a way to fully instantiate names in a given env/context

onInductiveIndexInside :: TypeX α Variable -> Φii α Variable -> TermX α Variable
onInductiveIndexInside t (α, b, _) =
  case unBinder b of
  Nothing -> error "onInductiveIndexInside: Nothing"
  Just v -> App α t (Var Nothing v)

onInductiveIndexOutside :: Φii α Variable -> TypeX α Variable -> TermX α Variable
onInductiveIndexOutside (α, b, i) t = Pi α i (abstractBinder b t)

onInductiveParameter :: TypeX α Variable -> Φip α Variable -> TermX α Variable
onInductiveParameter t (α, b, _) = App α t (Var Nothing b)

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

  foldrWith onInductiveIndexOutside inductiveIndices
  $ Pi α inductive (abstractAnonymous universe)

  where

    inductive :: TypeX α Variable
    inductive =
        foldlWith onInductiveIndexInside inductiveIndices
      $ foldlWith onInductiveParameter   inductiveParameters
      $ Var Nothing inductiveName

mkMotiveType :: ∀ α.
  α ->
  Inductive α Variable ->
  TypeX α Variable ->
  TypeX α Variable
mkMotiveType α (Inductive n ips iis _ _) universe =
  mkMotiveType' α n ips iis universe
  -- mkMotiveType' α n ps (instantiateBinders "i" is) universe
