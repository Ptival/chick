{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.AndTypeDeclaration
  ( and_type_declaration_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.Tokens
import Parsing.OCaml.TypeKind
import Parsing.OCaml.PostItemAttributes

and_type_declaration_P :: Parser Structure -> Parser Type_declaration
and_type_declaration_P structure_P = do
  try $ and_T
  -- TODO: attributes
  -- TODO: optional_type_parameters
  i <- l_ident_T
  (kind, priv, manifest) <- type_kind_P
  -- TODO: constraints
  _a <- post_item_attributes_P structure_P -- FIXME: use me
  return $ mkType [] [] kind priv manifest (mkRHS i 4) -- FIXME: [] and []
