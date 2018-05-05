{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.CoreTypeNoAttr
  ( core_type_no_attr_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.CoreType2

core_type_no_attr_P :: Parser Core_type
core_type_no_attr_P = choice
  [ core_type2_P
  -- , TODO
  ]
