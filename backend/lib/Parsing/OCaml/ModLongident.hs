module Parsing.OCaml.ModLongident
  ( mod_longident_P
  ) where

import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Tokens
import Parsing.Utils

mod_longident_P :: Parser Longident
mod_longident_P = chainl1try' u_ident_T (dot_T *> return Ldot) Lident
