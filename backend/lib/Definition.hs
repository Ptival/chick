{-# LANGUAGE FlexibleInstances #-}

module Definition
  ( Definition (..),
  )
where

import DefinitionObjectKind (DefinitionObjectKind)
import GHC.Generics (Generic)
import PrettyPrinting.Term ()
import Term.Term (TermX, TypeX, Variable)

data Definition α ν = Definition
  { definitionKind :: DefinitionObjectKind,
    definitionName :: ν,
    definitionType :: TypeX α ν,
    definitionTerm :: TermX α ν
  }
  deriving (Generic, Show)

deriving instance (Eq α) => Eq (Definition α Variable)
