{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.SeqExpr
  ( seq_expr_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.Expr
import Parsing.OCaml.Tokens

seq_expr_P :: Parser Expression
seq_expr_P = choice
  [ do
    e <- expr_P
    semi_T
    s <- seq_expr_P
    return $ mkExp Nothing Nothing (Pexp_sequence e s)
  , do
    e <- expr_P
    semi_T
    return e
  , expr_P
    -- TODO: percent
  ]
