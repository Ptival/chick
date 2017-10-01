module Term.Universe
  ( Universe(..)
  ) where

data Universe
  = Prop
  | Set
  | Type
  deriving (Eq, Show)
