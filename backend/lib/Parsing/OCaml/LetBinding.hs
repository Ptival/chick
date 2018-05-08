{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.LetBinding
  ( let_binding_P
  ) where

import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.LetBindingBody
import Parsing.OCaml.PostItemAttributes
import Parsing.OCaml.RecFlag
import Parsing.OCaml.Tokens

let_binding_P :: Parser Structure -> Parser Let_bindings
let_binding_P structure_P = do
  let_T
  -- TODO: ext_attributes
  r <- rec_flag_P
  b <- let_binding_body_P
  a <- post_item_attributes_P structure_P
  return $ mklbs Nothing r (mklb True b []) -- FIXME
