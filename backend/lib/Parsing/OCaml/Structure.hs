{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.Structure
  ( structure_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.PostItemAttributes
import Parsing.OCaml.SeqExpr
import Parsing.OCaml.StructureItem
import Parsing.OCaml.Tokens
import Parsing.OCaml.Utils

structure_P :: Parser Structure
structure_P = choice
  [ do
    e <- seq_expr_P
    a <- post_item_attributes_P structure_P
    s <- structure_tail_P
    return $ text_str 1 ++ mkstrexp e a : s
  , structure_tail_P
  ]
  where
    structure_tail_P :: Parser [Structure_item]
    structure_tail_P = choice
      [ do
        (i, t) <- try $ do
          i <- structure_item_P structure_P
          t <- structure_tail_P
          return (i, t)
        return $ text_str 1 ++ i : t
      , do
        try $ semi_semi_T
        s <- structure_P
        return $ text_str 1 ++ s
      , ocamlSpace *> return []
      ]
