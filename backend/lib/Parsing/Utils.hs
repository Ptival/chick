module Parsing.Utils
  ( chainl1
  ) where

import           Control.Applicative

chainl1 :: (Alternative m, Monad m) => m a -> m (a -> a -> a) -> m a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = do { f <- op ; y <- p ; rest (f x y) } <|> return x

-- chainl1' :: Parser a -> Parser (b -> a -> b) -> (a -> b) -> Parser b
-- chainl1' parser op baseConstructor = parser >>= (rest . baseConstructor)
--     where rest context = do
--             f <- op
--             y <- parser
--             rest $ f context y
--               <|> return context
