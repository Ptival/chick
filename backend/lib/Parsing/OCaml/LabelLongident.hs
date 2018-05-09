module Parsing.OCaml.LabelLongident
  ( label_longident_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.ModLongident
import Parsing.OCaml.Tokens

label_longident_P :: Parser Longident
label_longident_P = choice
  [ Lident <$> l_ident_T
  , do
    m <- mod_longident_P
    dot_T
    i <- l_ident_T
    return $ Ldot m i
  ]
