{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.MatchCases
  ( match_cases_P
  ) where

import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.MatchCase
import Parsing.OCaml.Tokens
import Parsing.Utils

match_cases_P :: Parser Expression -> Parser [Case]
match_cases_P seq_expr_P =
  chainl1' (match_case_P seq_expr_P) (bar_T *> return (flip (:))) (: [])
