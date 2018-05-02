{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Script
  ( Script(..)
  ) where

import Term.Variable
import Vernacular

data Script α ν = Script [Vernacular α ν]
  deriving (Show)

deriving instance (Eq α) => Eq (Script α Variable)
