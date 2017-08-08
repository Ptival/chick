module Diff.Atom
  ( Diff(..)
  , patch
  ) where

import Control.Monad.Freer

data Diff a
  = Same
  | Replace a
  deriving (Show)

patch :: a -> Diff a -> Eff r a
patch a d = case d of
  Same       -> return a
  Replace a' -> return a'
