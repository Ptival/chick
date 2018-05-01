{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Definition
  ( Definition(..)
  ) where

import           DefinitionObjectKind (DefinitionObjectKind)
import           PrettyPrinting.Term ()
import           Term.Term

data Definition α ν = Definition
  { definitionKind :: DefinitionObjectKind
  , definitionName :: ν
  , definitionType :: TypeX α ν
  , definitionTerm :: TermX α ν
  }
  deriving (Show)

deriving instance (Eq α) => Eq (Definition α Variable)
