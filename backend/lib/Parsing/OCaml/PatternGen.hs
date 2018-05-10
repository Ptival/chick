{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.PatternGen
  ( pattern_gen_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.ConstrLongident
import Parsing.OCaml.SimplePattern

pattern_gen_P :: Parser Pattern -> Parser Pattern
pattern_gen_P pattern_P = choice
  [ try $ do
    i <- constr_longident_P
    p <- pattern_P
    return $ mkpat $ Ppat_construct (mkRHS i 1) (Just p)
  , try $ simple_pattern_P
  ]
