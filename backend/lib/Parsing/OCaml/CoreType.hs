{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.CoreType
  ( core_type_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.CoreTypeNoAttr

core_type_P :: Parser Core_type
core_type_P = choice
  [ core_type_no_attr_P
  -- , do
  --   t <- core_type
  --   a <- attribute
  --   return $ attr t a
  ]
