module Parsing.OCaml.PostItemAttribute
  ( post_item_attribute_P
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.String

import qualified OCaml.Parsing.ASTTypes as ASTTypes
import           OCaml.Parsing.ParseTree
import           Parsing.OCaml.AttrId
import           Parsing.OCaml.Payload
import           Parsing.OCaml.Tokens

post_item_attribute_P :: Parser Structure -> Parser (ASTTypes.Loc String, Payload)
post_item_attribute_P structure_P = do
  try $ l_bracket_at_at_T
  i <- attr_id_P
  p <- payload_P structure_P
  r_bracket_T
  return (i, p)
