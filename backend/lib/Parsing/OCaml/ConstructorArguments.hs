{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.ConstructorArguments
  ( constructor_arguments_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.CoreTypeList

constructor_arguments_P :: Parser Constructor_arguments
constructor_arguments_P = choice
  [ Pcstr_tuple . reverse <$> core_type_list_P
  -- TODO: label declarations
  ]
