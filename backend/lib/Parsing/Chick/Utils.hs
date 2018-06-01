module Parsing.Chick.Utils
  ( identifier
  , lexeme
  , ocamlSpace
  , parens
  , rword
  , symbol
  ) where

import           Data.Functor
import           Parsing.Types
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x =
      if x `elem` reservedWords
      then fail $ printf "keyword %s cannot be an identifier" (show x)
      else return x

ocamlSpace :: Parser ()
ocamlSpace = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "(*" "*)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ocamlSpace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> ocamlSpace

symbol :: String -> Parser ()
symbol = void . L.symbol ocamlSpace

reservedWords :: [String] -- list of reserved words
reservedWords =
  [ "end"
  , "in"
  , "let"
  , "match"
  , "Prop"
  , "Set"
  , "Type"
  , "with"
  ]
