{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.AndTypeDeclaration
  ( and_type_declaration_P
  ) where

import Text.Megaparsec.String

and_type_declaration_P :: Parser a
and_type_declaration_P = do
  error "TODO"
