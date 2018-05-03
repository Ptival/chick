{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.SimpleCoreType
  ( simple_core_type_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.SimpleCoreType2

simple_core_type_P :: Parser Core_type
simple_core_type_P = choice
  [ simple_core_type2_P
  -- , parens core_type_comma_list
  ]
