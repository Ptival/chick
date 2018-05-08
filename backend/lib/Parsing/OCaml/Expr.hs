{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.Expr
  ( expr_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.MatchCases
import Parsing.OCaml.OptBar
import Parsing.OCaml.Tokens

expr_P :: Parser Expression -> Parser Expression
expr_P seq_expr_P = choice
  [ do
    try $ function_T
    -- TODO: ext_attributes
    opt_bar_P
    l <- match_cases_P seq_expr_P
    return $ mkexp_attrs (Pexp_function (reverse l)) (error "TODO")
  ]
