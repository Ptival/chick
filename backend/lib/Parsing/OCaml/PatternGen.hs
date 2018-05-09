{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.PatternGen
  ( pattern_gen_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.SimplePattern

pattern_gen_P :: Parser Pattern
pattern_gen_P = choice
  [ simple_pattern_P
  ]
