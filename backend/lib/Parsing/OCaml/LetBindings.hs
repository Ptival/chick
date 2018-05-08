{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.LetBindings
  ( let_bindings_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
-- import Parsing.OCaml.Attributes
import Parsing.OCaml.LetBinding
import Parsing.OCaml.LetBindingBody
import Parsing.OCaml.PostItemAttributes
import Parsing.OCaml.Tokens
import Parsing.Utils

let_bindings_P :: Parser Structure -> Parser Let_bindings
let_bindings_P structure_P = leftRecursive
  [ let_binding_P structure_P
  ]
  [ do
    try $ and_T
    -- a <- attributes_P
    b <- let_binding_body_P
    p <- post_item_attributes_P structure_P
    return $ \ x -> addlb x (mklb False b []) -- FIXME
  ]
