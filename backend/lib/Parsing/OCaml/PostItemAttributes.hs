{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.PostItemAttributes
  ( post_item_attributes_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.Common
import Parsing.OCaml.ConstructorArguments
import Parsing.OCaml.Tokens
import Parsing.OCaml.TypeDeclarations

post_item_attributes_P :: Parser [a]
post_item_attributes_P = many post_item_attribute_P
