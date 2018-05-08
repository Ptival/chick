{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.LetBindingBody
  ( let_binding_body_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.StrictBinding
import Parsing.OCaml.ValIdent

let_binding_body_P :: Parser (Pattern, Expression)
let_binding_body_P = choice
  [ do
    i <- val_ident_P
    b <- strict_binding_P
    return $ (mkpatvar i 1, b)
  ]
