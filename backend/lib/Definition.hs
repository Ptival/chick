{-# LANGUAGE FlexibleInstances #-}

module Definition (
  Definition(..),
  ) where

import GHC.Generics

import DefinitionObjectKind (DefinitionObjectKind)
import PrettyPrinting.Term ()
import Term.Term

data Definition α ν = Definition
  { definitionKind :: DefinitionObjectKind
  , definitionName :: ν
  , definitionType :: TypeX α ν
  , definitionTerm :: TermX α ν
  }
  deriving (Generic, Show)

deriving instance (Eq α) => Eq (Definition α Variable)
