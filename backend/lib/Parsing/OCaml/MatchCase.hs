{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.MatchCase
  ( match_case_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.Pattern
import Parsing.OCaml.Tokens

match_case_P :: Parser Expression -> Parser Case
match_case_P seq_expr_P = choice
  [ do
    p <- try $ do
      p <- pattern_P
      minus_greater_T
      return p
    e <- seq_expr_P
    return $ caseExp p Nothing e
  -- TODO: others
  ]
