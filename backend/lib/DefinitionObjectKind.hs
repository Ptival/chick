{-# LANGUAGE FlexibleInstances #-}

module DefinitionObjectKind
  ( DefinitionObjectKind(..)
  ) where

data DefinitionObjectKind
  = Definition
  | Fixpoint
  deriving (Eq, Show)
