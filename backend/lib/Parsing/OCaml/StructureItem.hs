{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.StructureItem
  ( structure_item_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.TypeDeclarations

structure_item_P :: Parser Structure -> Parser Structure_item
structure_item_P structure_P = choice
  [ do
    (nr, l) <- type_declarations_P structure_P
    return $ mkstr_ext (Pstr_type nr (reverse l)) Nothing -- FIXME: not Nothing
  ]
