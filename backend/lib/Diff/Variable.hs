module Diff.Variable
  ( Diff (..),
  )
where

import Term.Variable (Variable)

data Diff
  = Same
  | Renamed Variable
