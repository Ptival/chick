module Diff.Atom
  ( Diff(..)
  , patch
  ) where

import Control.Monad.Freer

data Diff a
  = Same
  | Change a
  deriving (Show)

patch :: a -> Diff a -> Eff r a
patch a d = case d of
  Same      -> return a
  Change a' -> return a'
