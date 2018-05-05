{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.SimpleCoreTypeOrTuple
  ( simple_core_type_or_tuple_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.SimpleCoreType

simple_core_type_or_tuple_P :: Parser Core_type
simple_core_type_or_tuple_P = choice
  [ simple_core_type_P
  -- TODO
  ]
