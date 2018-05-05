{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.CoreType2
  ( core_type2_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.SimpleCoreTypeOrTuple

core_type2_P :: Parser Core_type
core_type2_P = choice
  [ simple_core_type_or_tuple_P
  -- TODO
  ]
