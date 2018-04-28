{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module DefinitionObjectKind
  ( DefinitionObjectKind(..)
  ) where

data DefinitionObjectKind
  = Definition
  | Fixpoint
  deriving (Eq, Show)
