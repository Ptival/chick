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

leftRecursive :: (Monad m, Alternative m) => [m a] -> [m (a -> a)] -> m a
leftRecursive prefixes suffixes = choice $ map patchPrefix prefixes
  where
    patchPrefix prefixP = do
      p <- prefixP
      r <- rest
      return $ r p
    patchSuffix suffixP = do
      s <- suffixP
      r <- rest
      return $ r . s
    rest = choice $ map patchSuffix suffixes ++ [ return id ]
