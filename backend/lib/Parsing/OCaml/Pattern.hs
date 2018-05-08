{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.Pattern
  ( pattern_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.PatternCommaList
import Parsing.OCaml.Tokens
import Parsing.OCaml.ValIdent

pattern_P :: Parser Pattern
pattern_P = choice
  [ do
    l <- pattern_comma_list_P pattern_P
    r <- rest
    return . r . mkpat $ Ppat_tuple (reverse l)
  ]
  where
    rest = choice
      [ do
        try $ as_T
        i <- val_ident_P
        r <- rest
        return $ \ x -> r $ mkpat $ Ppat_alias x (mkRHS i 3)
      -- TODO: pattern COLONCOLON pattern
      , do
        try $ bar_T
        p <- pattern_P
        r <- rest
        return $ \ x -> r $ mkpat $ Ppat_or x p
      -- TODO: bunch of other patterns
      , return id
      ]
