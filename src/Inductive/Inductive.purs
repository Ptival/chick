module Inductive.Inductive where

import Prelude
import Data.List (List, foldr)
import Inductive.Constructor (BoundTerm, BoundType, Constructor)
import Term.Term (TermX(..), TypeX, abstractAnonymous, abstractBinder)
import Term.Variable (Variable)

data Inductive α ν =
  Inductive
  { name         :: ν
  , parameters   :: List (BoundType α ν)
  , indices      :: List (TypeX α ν)
  , constructors :: List (Constructor α ν)
  }

inductiveType ::
  ∀ α.
  α ->
  List (BoundType α Variable) ->
  List (TypeX α Variable) ->
  TypeX α Variable ->
  TypeX α Variable
inductiveType def ps is o =
  o
  #   flip (foldr onIndex) is
  >>> flip (foldr onParam) ps

  where

    onIndex :: TypeX α Variable -> TypeX α Variable -> TypeX α Variable
    onIndex i t = Pi def i (abstractAnonymous t)

    onParam :: BoundTerm α Variable -> TypeX α Variable -> TypeX α Variable
    onParam { binder, term } t = Pi def term (abstractBinder binder t)
