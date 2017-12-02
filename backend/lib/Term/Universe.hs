{-# LANGUAGE DeriveGeneric #-}

module Term.Universe
  ( Universe(..)
  ) where

import Data.Aeson
import GHC.Generics

data Universe
  = Prop
  | Set
  | Type
  deriving (Eq, Generic, Show)

instance ToJSON Universe where
