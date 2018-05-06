{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.StructureItem
  ( structure_item_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.OpenStatement
import Parsing.OCaml.TypeDeclarations

structure_item_P :: Parser Structure -> Parser Structure_item
structure_item_P structure_P = choice
  [ do
    (nr, l) <- try $ type_declarations_P structure_P
    return $ mkstr_ext (Pstr_type nr (reverse l)) Nothing -- FIXME: not Nothing
  , do
    (body, ext) <- open_statement_P structure_P
    return $ mkstr_ext (Pstr_open body) Nothing -- FIXME
  ]
