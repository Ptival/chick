{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.PatternCommaList
  ( pattern_comma_list_P
  ) where

import Text.Megaparsec.String

import Parsing.OCaml.Tokens
import Parsing.Utils

pattern_comma_list_P :: Parser a -> Parser [a]
pattern_comma_list_P pattern_P =
  chainl1' pattern_P (comma_T *> return (flip (:))) (: [])
