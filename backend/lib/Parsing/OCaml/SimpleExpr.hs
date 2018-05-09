{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.SimpleExpr
  ( simple_expr_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.Constant
import Parsing.OCaml.ValLongident

simple_expr_P :: Parser Expression
simple_expr_P = choice
  [ do
    i <- val_longident_P
    return . mkexp $ Pexp_ident (mkRHS i 1)
  , mkexp . Pexp_constant <$> constant_P
  ]
