module Parsing.Utils
  ( chainl1
  , identifier
  , lexeme
  , parens
  , rword
  , symbol
  ) where

import           Control.Applicative
import           Data.Functor
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String
import           Text.Printf

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

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt  = empty -- L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "(*" "*)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser ()
symbol = void . L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

reservedWords :: [String] -- list of reserved words
reservedWords =
  [ "λ"
  , "end"
  , "in"
  , "Inductive"
  , "let"
  , "match"
  , "with"
  ]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x =
      if x `elem` reservedWords
      then fail $ printf "keyword %s cannot be an identifier" (show x)
      else return x
