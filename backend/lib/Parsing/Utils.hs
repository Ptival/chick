module Parsing.Utils
  ( chainl1
  , chainl1'
  , leftRecursive
  ) where

import Control.Applicative
import Text.Megaparsec

chainl1' :: (Alternative m, Monad m) => m a -> m (b -> a -> b) -> (a -> b) -> m b
chainl1' p op bc = p >>= rest . bc
  where
    rest x = do { f <- op ; y <- p ; rest (f x y) } <|> return x

chainl1 :: (Alternative m, Monad m) => m a -> m (a -> a -> a) -> m a
chainl1 p op = chainl1' p op id

leftRecursive :: (Alternative m, Monad m) => m b -> m (b -> b) -> m b
leftRecursive prefixP suffixP = do
  p <- prefixP
  r <- rest
  return $ r p
  where
    rest = choice
      [ do
        s <- suffixP
        r <- rest
        return $ r . s
      , return id
      ]
