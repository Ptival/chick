module Inductive.Constructor where

import Prelude
import Term.Raw as Raw
import Data.List (List, foldr)
import Data.Maybe (Maybe(..))
import Term.Binder (Binder(..))
import Term.Term (TermX(..), TypeX, abstractBinder)
import Term.Variable (Variable)

type BoundTerm α ν =
  { binder :: Binder ν
  , term   :: TermX α ν
  }

type BoundType α ν = BoundTerm α ν

data Constructor α ν =
  Constructor
  { name       :: ν
  , parameters :: BoundType α ν
  , indices    :: List (TypeX α ν)
  }

rawConstructorType :: ∀ α.
  Variable ->
  List (BoundTerm α Variable) ->
  List (BoundTerm α Variable) ->
  List (TermX α Variable) ->
  TypeX Unit Variable
rawConstructorType ind indps ps is =
  Var ind                           -- start with the inductive type name
  #   flip (foldr onIndParam) indps -- add the inductive parameters
  >>> flip (foldr onIndex)    is    -- add the constructor parameters
  >>> flip (foldr onParam)    ps    -- add the constructor indices

  where

    onIndex :: TermX α Variable -> Raw.Term Variable -> Raw.Term Variable
    onIndex i t = App unit (Raw.raw t) (Raw.raw i)

    onParam :: BoundTerm α Variable -> Raw.Term Variable -> Raw.Term Variable
    onParam { binder, term } t = Pi unit (Raw.raw term) (abstractBinder binder t)

    onIndParam :: BoundTerm α Variable -> Raw.Term Variable -> Raw.Term Variable
    onIndParam { binder : Binder (Just v) } t = App unit (Raw.raw t) (Var v)
    onIndParam { binder : Binder Nothing  } t = App unit (Raw.raw t) (Hole unit)
