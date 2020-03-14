{-# LANGUAGE FlexibleInstances #-}

module Script
  ( Script(..)
  ) where

import Data.List

import Term.Variable
import Vernacular

newtype Script α ν = Script { unScript :: [Vernacular α ν] }

instance (Show α, Show ν) => Show (Script α ν) where
  show (Script l) = intercalate "\n" $ map show l

deriving instance (Eq α) => Eq (Script α Variable)
