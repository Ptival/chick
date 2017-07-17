module Diff.Variable
  ( Diff(..)
  ) where

import Term.Variable

data Diff
  = Same
  | Renamed Variable
