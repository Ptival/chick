{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.ValLongident
  ( val_longident_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.ModLongident
import Parsing.OCaml.Tokens
import Parsing.OCaml.ValIdent

val_longident_P :: Parser Longident
val_longident_P = choice
  [ Lident <$> val_ident_P
  , do
    l <- mod_longident_P
    dot_T
    i <- val_ident_P
    return $ Ldot l i
  ]
