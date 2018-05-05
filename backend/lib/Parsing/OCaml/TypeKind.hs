{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.TypeKind
  ( type_kind_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.ConstructorDeclarations
import Parsing.OCaml.CoreType
import Parsing.OCaml.LabelDeclarations
import Parsing.OCaml.PrivateFlag
import Parsing.OCaml.Tokens

type_kind_P :: Parser (Type_kind, Private_flag, Maybe Core_type)
type_kind_P = choice
  [ do
    t <- try $ do
      equal_T
      core_type_P
    return (Ptype_abstract, Public, Just t)
  , do
    cs <- try $ do
      equal_T
      constructor_declarations_P
    return (Ptype_variant (reverse cs), Private, Nothing)
  , do
    priv <- try $ do
      equal_T
      priv <- private_flag_P
      l_brace_T
      return priv
    labels <- label_declarations_P
    r_brace_T
    return (Ptype_record labels, priv, Nothing)
  , return (Ptype_abstract, Public, Nothing)
  ]
