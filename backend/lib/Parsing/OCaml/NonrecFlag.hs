{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.NonrecFlag
  ( nonrec_flag_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Tokens

nonrec_flag_P :: Parser Rec_flag
nonrec_flag_P = choice
  [ nonrec_T *> return Nonrecursive
  , return Recursive
  ]
