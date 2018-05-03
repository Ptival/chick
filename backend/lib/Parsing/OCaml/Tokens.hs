module Parsing.OCaml.Tokens
  ( bar_T
  , colon_T
  , dot_T
  , equal_T
  , false_T
  , l_brace_T
  , l_bracket_T
  , l_ident_T
  , mutable_T
  , of_T
  , private_T
  , quote_T
  , r_brace_T
  , r_bracket_T
  , semi_T
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

colon_T :: Parser ()
colon_T = symbol ":"

dot_T :: Parser ()
dot_T = symbol "."

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

l_brace_T :: Parser ()
l_brace_T = symbol "{"

l_bracket_T :: Parser ()
l_bracket_T = symbol "["

l_ident_T :: Parser String
l_ident_T = lexeme . try $ do
  c <- lowerChar
  cs <- many ident_char_T
  return $ c : cs

mutable_T :: Parser ()
mutable_T = rword "mutable"

of_T :: Parser ()
of_T = rword "of"

private_T :: Parser ()
private_T = rword "private"

quote_T :: Parser ()
quote_T = symbol "'"

r_brace_T :: Parser ()
r_brace_T = symbol "}"

r_bracket_T :: Parser ()
r_bracket_T = symbol "]"

semi_T :: Parser ()
semi_T = symbol ";"

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
