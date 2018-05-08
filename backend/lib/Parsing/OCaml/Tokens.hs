module Parsing.OCaml.Tokens
  ( and_T
  , as_T
  , bang_T
  , bar_T
  , colon_T
  , comma_T
  , dot_T
  , equal_T
  , false_T
  , function_T
  , l_brace_T
  , l_bracket_T
  , l_bracket_at_at_T
  , l_ident_T
  , l_paren_T
  , let_T
  , minus_greater_T
  , module_T
  , mutable_T
  , nonrec_T
  , of_T
  , open_T
  , private_T
  , quote_T
  , r_brace_T
  , r_bracket_T
  , r_paren_T
  , rec_T
  , semi_T
  , semi_semi_T
  , star_T
  , true_T
  , type_T
  , u_ident_T
  , underscore_T
  ) where

import Parsing.OCaml.Utils
import Text.Megaparsec
import Text.Megaparsec.String

and_T :: Parser ()
and_T = rword "and"

as_T :: Parser ()
as_T = rword "as"

bang_T :: Parser ()
bang_T = symbol "!"

bar_T :: Parser ()
bar_T = symbol "|"

colon_T :: Parser ()
colon_T = symbol ":"

comma_T :: Parser ()
comma_T = symbol ","

dot_T :: Parser ()
dot_T = symbol "."

equal_T :: Parser ()
equal_T = symbol "="

false_T :: Parser ()
false_T = rword "false"

function_T :: Parser ()
function_T = rword "function"

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

l_bracket_at_at_T :: Parser ()
l_bracket_at_at_T = symbol "[@@"

l_ident_T :: Parser String
l_ident_T = lexeme . try $ do
  c <- lowerChar
  cs <- many ident_char_T
  return $ c : cs

l_paren_T :: Parser ()
l_paren_T = symbol "("

let_T :: Parser ()
let_T = rword "let"

nonrec_T :: Parser ()
nonrec_T = rword "nonrec"

minus_greater_T :: Parser ()
minus_greater_T = symbol "->"

module_T :: Parser ()
module_T = rword "module"

mutable_T :: Parser ()
mutable_T = rword "mutable"

of_T :: Parser ()
of_T = rword "of"

open_T :: Parser ()
open_T = rword "open"

private_T :: Parser ()
private_T = rword "private"

quote_T :: Parser ()
quote_T = symbol "'"

r_brace_T :: Parser ()
r_brace_T = symbol "}"

r_bracket_T :: Parser ()
r_bracket_T = symbol "]"

r_paren_T :: Parser ()
r_paren_T = symbol ")"

rec_T :: Parser ()
rec_T = rword "rec"

semi_T :: Parser ()
semi_T = symbol ";"

semi_semi_T :: Parser ()
semi_semi_T = symbol ";;"

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
