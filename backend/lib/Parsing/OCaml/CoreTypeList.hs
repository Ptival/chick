{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.CoreTypeList
  ( core_type_list_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.SimpleCoreType
import Parsing.OCaml.Tokens

core_type_list_P :: Parser [Core_type]
core_type_list_P = simple_core_type_P `sepBy` star_T
