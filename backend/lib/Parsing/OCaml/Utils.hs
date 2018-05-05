module Parsing.OCaml.Utils
  ( identifier
  , lexeme
  , ocamlSpace
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
  [ "and"
  , "as"
  , "assert"
  , "begin"
  , "class"
  , "constraint"
  , "do"
  , "done"
  , "downto"
  , "else"
  , "end"
  , "exception"
  , "external"
  , "false"
  , "for"
  , "fun"
  , "function"
  , "functor"
  , "if"
  , "in"
  , "include"
  , "inherit"
  , "initializer"
  , "lazy"
  , "let"
  , "match"
  , "method"
  , "module"
  , "mutable"
  , "new"
  , "nonrec"
  , "object"
  , "of"
  , "open"
  , "or"
  -- , "parser"
  , "private"
  , "rec"
  , "sig"
  , "struct"
  , "then"
  , "to"
  , "true"
  , "try"
  , "type"
  , "val"
  , "virtual"
  , "when"
  , "while"
  , "with"
  , "lor"
  , "lxor"
  , "mod"
  , "land"
  , "lsl"
  , "lsr"
  , "asr"
  ]
