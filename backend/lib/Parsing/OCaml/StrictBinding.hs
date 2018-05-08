{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.StrictBinding
  ( strict_binding_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.SeqExpr
import Parsing.OCaml.Tokens

strict_binding_P :: Parser Expression
strict_binding_P  = choice
  [ do
    try $ equal_T
    seq_expr_P
    -- TODO
  ]
