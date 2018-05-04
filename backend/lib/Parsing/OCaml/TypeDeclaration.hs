{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.TypeDeclaration
  ( type_declaration_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.Common
import Parsing.OCaml.NonrecFlag
import Parsing.OCaml.Tokens
import Parsing.OCaml.TypeKind

type_declaration_P :: Parser (Rec_flag, Type_declaration)
type_declaration_P = do
  try $ type_T
  -- TODO: ext_attributes
  nonrec_flag <- nonrec_flag_P
  -- TODO: optional_type_parameters
  n <- l_ident_T
  (kind, priv, manifest) <- type_kind_P
  -- TODO: constraints
  -- TODO: post_item_attributes
  let ty = mkType [] [] kind priv manifest (mkRHS n 5)
  return (nonrec_flag, ty)
