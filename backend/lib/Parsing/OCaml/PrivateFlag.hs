{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.PrivateFlag
  ( private_flag_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.Tokens

private_flag_P :: Parser Private_flag
private_flag_P = choice
  [ private_T *> return Private
  , return Public
  ]
