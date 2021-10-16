module Diff.Guess.HIList
  ( HIList (..),
    insert,
    peekMax,
  )
where

import Data.List (insertBy)
import Data.Ord (Down (Down), comparing)
import Diff.Guess.Node (Node (height))

-- HI stands for height-indexed (invariant: decreasing heights)
newtype HIList = HIList {unHIList :: [Node]}

instance Show HIList where
  show = show . unHIList

peekMax :: HIList -> Int
peekMax (HIList []) = 0
peekMax (HIList (h : _)) = height h

insert :: Node -> HIList -> HIList
insert n = HIList . insertBy (comparing $ Down . height) n . unHIList
