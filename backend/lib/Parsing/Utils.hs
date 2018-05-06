module Parsing.Utils
  ( chainl1
  , chainl1'
  ) where

import Control.Applicative

chainl1' :: (Alternative m, Monad m) => m a -> m (b -> a -> b) -> (a -> b) -> m b
chainl1' p op bc = p >>= rest . bc
  where
    rest x = do { f <- op ; y <- p ; rest (f x y) } <|> return x

chainl1 :: (Alternative m, Monad m) => m a -> m (a -> a -> a) -> m a
chainl1 p op = chainl1' p op id
