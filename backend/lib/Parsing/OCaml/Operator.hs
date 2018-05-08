{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.Operator
  ( operator_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Parsing.OCaml.Tokens

operator_P :: Parser String
operator_P = choice
  [
    -- TODO
    bang_T *> return "!"
  ]
