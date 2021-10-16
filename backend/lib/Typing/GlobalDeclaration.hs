{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Typing.GlobalDeclaration
  ( GlobalDeclaration (..),
    nameOf,
  )
where

import Inductive.Inductive (Inductive (inductiveName))
import Term.Term (TermX, TypeX, Variable)

data GlobalDeclaration α ν
  = GlobalAssum ν (TypeX α ν)
  | -- careful, the term and type arguments are convertible
    -- v : τ := t
    GlobalDef
      { globalDefName :: ν,
        globalDefType :: TypeX α ν,
        globalDefDefn :: TermX α ν
      }
  | GlobalInd (Inductive α ν)

deriving instance (Eq α) => Eq (GlobalDeclaration α Variable)

deriving instance (Show α, Show ν) => Show (GlobalDeclaration α ν)

nameOf :: GlobalDeclaration α ν -> ν
nameOf (GlobalAssum v _) = v
nameOf (GlobalDef v _ _) = v
nameOf (GlobalInd ind) = inductiveName ind
