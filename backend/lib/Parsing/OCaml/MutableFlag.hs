{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.MutableFlag
  ( mutable_flag_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Tokens

mutable_flag_P :: Parser Mutable_flag
mutable_flag_P = choice
  [ mutable_T *> return Mutable
  , return Immutable
  ]
