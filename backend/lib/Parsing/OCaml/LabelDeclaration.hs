{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.LabelDeclaration
  ( label_declaration_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.Common
import Parsing.OCaml.Label
import Parsing.OCaml.MutableFlag
import Parsing.OCaml.PolyTypeNoAttr
import Parsing.OCaml.Tokens

label_declaration_P :: Parser Label_declaration
label_declaration_P = try $ do
  mut <- mutable_flag_P
  label <- label_P
  colon_T
  t <- poly_type_no_attr_P
  -- TODO: attributes
  return $ field mut (mkRHS label 2) t
