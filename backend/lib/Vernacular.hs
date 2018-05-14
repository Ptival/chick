{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Vernacular
  ( Vernacular(..)
  ) where

import GHC.Generics
import Language.OCaml.Definitions.Parsing.ParseTree

import qualified Definition as D
import qualified Inductive.Inductive as I
import           Term.Term

data Vernacular α ν
  = Definition  (D.Definition α ν)
  | Inductive   (I.Inductive α ν)
  | UnsupportedOCaml Structure_item
  deriving (Generic, Show)

deriving instance (Eq α) => Eq (Vernacular α Variable)
