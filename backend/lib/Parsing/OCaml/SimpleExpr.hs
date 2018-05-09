{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.SimpleExpr
  ( simple_expr_P
  ) where

import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.Constant
import Parsing.OCaml.ConstrLongident
import Parsing.OCaml.LabelLongident
import Parsing.OCaml.ValLongident
import Parsing.OCaml.Tokens
import Parsing.Utils

simple_expr_P :: Parser Expression
simple_expr_P = leftRecursive
  [ do
    i <- val_longident_P
    return . mkexp $ Pexp_ident (mkRHS i 1)
  , mkexp . Pexp_constant <$> constant_P
  , do
    i <- constr_longident_P
    return $ mkexp $ Pexp_construct (mkRHS i 1) Nothing
  ]
  [ do
    dot_T
    i <- label_longident_P
    return $ \ x -> mkexp $ Pexp_field x (mkRHS i 3)
  ]
