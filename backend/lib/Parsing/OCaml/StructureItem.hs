{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.StructureItem
  ( structure_item_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.LetBindings
import Parsing.OCaml.ModuleBinding
import Parsing.OCaml.OpenStatement
import Parsing.OCaml.TypeDeclarations

structure_item_P :: Parser Structure -> Parser Structure_item
structure_item_P structure_P = choice
  [ val_of_let_bindings <$> let_bindings_P structure_P
  , do
    (nr, l) <- type_declarations_P structure_P
    return $ mkstr_ext (Pstr_type nr (reverse l)) Nothing -- FIXME: not Nothing
  , do
    (body, ext) <- open_statement_P structure_P
    return $ mkstr_ext (Pstr_open body) Nothing -- FIXME
  , do
    (body, ext) <- module_binding_P structure_P
    return $ mkstr_ext (Pstr_module body) Nothing -- FIXME
  ]
