{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.PolyTypeNoAttr
  ( poly_type_no_attr_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.CoreTypeNoAttr

poly_type_no_attr_P :: Parser Core_type
poly_type_no_attr_P = choice
  [ core_type_no_attr_P
  -- TODO
  ]
