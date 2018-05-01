{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Vernacular
  ( Vernacular(..)
  ) where

import qualified Definition as D
import qualified Inductive.Inductive as I
import           Term.Term

data Vernacular α ν
  = Definition  (D.Definition α ν)
  | Inductive   (I.Inductive α ν)
  | Unsupported String
  deriving (Show)

deriving instance (Eq α) => Eq (Vernacular α Variable)
