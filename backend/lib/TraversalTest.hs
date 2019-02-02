{-# LANGUAGE RecordWildCards #-}

-- |

module TraversalTest where

import Control.Lens

data ContainsMoreInts = ContainsMoreInts
  { yetAnotherInt  :: Int
  , yetAnotherBool :: Bool
  , oneLastInt     :: Int
  }
  deriving (Show)

data ContainsInts = ContainsInts
  { anInt            :: Int
  , aBool            :: Bool
  , anotherInt       :: Int
  , containsMoreInts :: ContainsMoreInts
  }
  deriving (Show)

t1 :: Traversal' ContainsMoreInts Int
t1 f (ContainsMoreInts {..}) =
  ContainsMoreInts
  <$> pure yetAnotherInt
  <*> pure yetAnotherBool
  <*> f oneLastInt

t2 :: Traversal' ContainsInts Int
t2 f (ContainsInts {..}) =
  ContainsInts
  <$> f    anInt
  <*> pure aBool
  <*> f    anotherInt
  <*> (traverseOf t1 f containsMoreInts) -- (_ f containsMoreInts)

beep :: ContainsMoreInts
beep = ContainsMoreInts 0 True 42

boop :: ContainsInts
boop = ContainsInts 7 True 10 beep

foo :: (Applicative f) => f ContainsMoreInts
foo = traverseOf t1 (\ i -> pure (i + 10)) beep

bar :: (Applicative f) => f ContainsInts
bar = traverseOf t2 (\ i -> pure (i + 9000)) boop
