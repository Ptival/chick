{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.TypeDeclarations
  ( type_declarations_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.Common
import Parsing.OCaml.Tokens
import Parsing.OCaml.TypeKind

type_declarations_P :: Parser [Type_declaration]
type_declarations_P = do
  TODO
