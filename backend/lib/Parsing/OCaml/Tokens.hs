module Parsing.OCaml.Tokens
  ( bar_T
  , equal_T
  , false_T
  , l_bracket_T
  , l_ident_T
  , of_T
  , quote_T
  , r_bracket_T
  , star_T
  , true_T
  , type_T
  , u_ident_T
  , underscore_T
  ) where

import Parsing.Utils
import Text.Megaparsec
import Text.Megaparsec.String

bar_T :: Parser ()
bar_T = symbol "|"

equal_T :: Parser ()
equal_T = symbol "="

false_T :: Parser ()
false_T = rword "false"

ident_char_T :: Parser Char
ident_char_T = choice
  [ upperChar
  , lowerChar
  , char '_'
  , char '\''
  , digitChar
  ]

l_bracket_T :: Parser Char
l_bracket_T = char '['

l_ident_T :: Parser String
l_ident_T = lexeme . try $ do
  c <- lowerChar
  cs <- many ident_char_T
  return $ c : cs

of_T :: Parser ()
of_T = rword "of"

quote_T :: Parser ()
quote_T = symbol "'"

r_bracket_T :: Parser Char
r_bracket_T = char ']'

star_T :: Parser ()
star_T = symbol "*"

true_T :: Parser ()
true_T = rword "true"

type_T :: Parser ()
type_T = rword "type"

u_ident_T :: Parser String
u_ident_T = lexeme . try $ do
  c <- upperChar
  cs <- many ident_char_T
  return $ c : cs

underscore_T :: Parser ()
underscore_T = symbol "_"
