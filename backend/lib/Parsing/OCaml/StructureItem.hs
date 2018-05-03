{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.StructureItem
  ( structure_item_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.ConstructorArguments
import Parsing.OCaml.Tokens
import Parsing.OCaml.TypeDeclarations

structure_item_P = choice
  [ type_declarations_P
  ]
