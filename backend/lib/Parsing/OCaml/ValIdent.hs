{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.ValIdent
  ( val_ident_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Parsing.OCaml.Operator
import Parsing.OCaml.Tokens

val_ident_P :: Parser String
val_ident_P = choice
  [ l_ident_T
  , do
    try $ l_paren_T
    o <- operator_P
    r_paren_T
    return o
  ]
