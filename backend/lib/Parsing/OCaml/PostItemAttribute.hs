module Parsing.OCaml.PostItemAttribute
  ( post_item_attribute_P
  ) where

import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.AttrId
import Parsing.OCaml.Payload
import Parsing.OCaml.Tokens

post_item_attribute_P :: Parser (Loc String, Payload)
post_item_attribute_P = do
  l_bracket_at_at_T
  i <- attr_id_P
  p <- payload_P
  r_bracket_T
  return (i, p)
