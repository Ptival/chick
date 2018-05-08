{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.ModuleBindingBody
  ( module_binding_body_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.ModuleExpr
import Parsing.OCaml.Tokens

module_binding_body_P :: Parser Module_expr
module_binding_body_P = choice
  [ do
    try $ equal_T
    module_expr_P
    -- TODO: rest
  ]
