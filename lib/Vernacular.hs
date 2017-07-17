module Vernacular
  ( Vernacular(..)
  ) where

import Inductive.Inductive
import Term.Term

data Vernacular α ν
  = Definition ν (TypeX α ν) (TermX α ν)
  | Inductive (Inductive α ν)
