{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.PatternCommaList
  ( pattern_comma_list_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Parsing.OCaml.Tokens

pattern_comma_list_P :: Parser a -> Parser [a]
pattern_comma_list_P pattern_P =
  try $ do
  p1 <- pattern_P
  comma_T
  p2 <- pattern_P
  r <- rest
  return $ r [p2, p1]
  where
    rest = choice
      [ try $ do
        comma_T
        p <- pattern_P
        r <- rest
        return $ \ x -> r $ p : x
      , return id
      ]
