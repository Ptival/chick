{-# LANGUAGE FlexibleInstances #-}

module Vernacular
  ( Vernacular (..),
  )
where

import qualified Definition as D
import GHC.Generics (Generic)
import qualified Inductive.Inductive as I
import Language.OCaml.Definitions.Parsing.ParseTree
  ( StructureItem,
  )
import Term.Variable (Variable)

data Vernacular α ν
  = Definition (D.Definition α ν)
  | Inductive (I.Inductive α ν)
  | UnsupportedOCaml StructureItem
  deriving (Generic, Show)

deriving instance (Eq α) => Eq (Vernacular α Variable)
