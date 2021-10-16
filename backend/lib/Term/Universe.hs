module Term.Universe
  ( Universe (..),
  )
where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

data Universe
  = Prop
  | Set
  | Type
  deriving (Eq, Generic, Show)

instance ToJSON Universe
