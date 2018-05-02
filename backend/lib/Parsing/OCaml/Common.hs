module Parsing.OCaml.Common
  ( constr_ident_P
  , ident_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Parsing.OCaml.Tokens

constr_ident_P :: Parser String
constr_ident_P = choice
  [ u_ident_T
  , l_bracket_T *> r_bracket_T *> return "[]"
  -- TODO: other ones
  , false_T *> return "false"
  , true_T *> return "true"
  ]

ident_P :: Parser String
ident_P = choice [ u_ident_T, l_ident_T ]
