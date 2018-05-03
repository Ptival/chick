{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.TypeLongident
  ( type_longident_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.ModExtLongident
import Parsing.OCaml.Tokens

type_longident_P :: Parser Longident
type_longident_P = choice
  [ Lident <$> l_ident_T
  , do
    p <- mod_ext_longident_P
    dot_T
    i <- l_ident_T
    return $ Ldot p i
  ]
