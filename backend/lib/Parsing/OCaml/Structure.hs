{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.Structure
  ( structure_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.Common
import Parsing.OCaml.ConstructorArguments
import Parsing.OCaml.PostItemAttributes
import Parsing.OCaml.SeqExpr
import Parsing.OCaml.Tokens
import Parsing.OCaml.TypeDeclarations

structure_P :: Parser Structure
structure_P = choice
  [ do
    e <- seq_expr_P
    a <-post_item_attributes_P
    s <- structure_tail_P
    return $ mkstrexp e a : s
  ]
  where
    structure_tail_P :: Parser [Structure_item]
    structure_tail_P = choice
      [ do
        i <- structure_item_P
        t <- structure_tail_P
        return $ i : t
      , do
        semi_semi_T
        s <- structure_P
        return s
      , return []
      ]
