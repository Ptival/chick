{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.SimplePatternNotIdent
  ( simple_pattern_not_ident_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.ConstrLongident
import Parsing.OCaml.Tokens

simple_pattern_not_ident_P :: Parser Pattern
simple_pattern_not_ident_P = choice
  [ do
    underscore_T
    return $ mkpat $ Ppat_any
  -- TODO: signed constants
  , do
    i <- constr_longident_P
    return $ mkpat $ Ppat_construct (mkRHS i 1) Nothing
  ]
