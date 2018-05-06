{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.PostItemAttributes
  ( post_item_attributes_P
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.String

import qualified OCaml.Parsing.ASTTypes as ASTTypes
import           OCaml.Parsing.ParseTree
import           Parsing.OCaml.PostItemAttribute

post_item_attributes_P :: Parser Structure -> Parser [(ASTTypes.Loc String, Payload)]
post_item_attributes_P structure_P = many (post_item_attribute_P structure_P)
