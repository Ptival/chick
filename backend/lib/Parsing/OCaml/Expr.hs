{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.Expr
  ( expr_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.Common
import Parsing.OCaml.ConstructorArguments
import Parsing.OCaml.Tokens
import Parsing.OCaml.TypeDeclarations

expr_P :: Parser a
expr_P = choice
  [
  ]
