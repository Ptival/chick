{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.SimplePattern
  ( simple_pattern_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.SimplePatternNotIdent
import Parsing.OCaml.ValIdent

simple_pattern_P :: Parser Pattern
simple_pattern_P = choice
  [ do
    i <- val_ident_P
    return $ mkpat $ Ppat_var $ mkRHS i 1
  , simple_pattern_not_ident_P
  ]
