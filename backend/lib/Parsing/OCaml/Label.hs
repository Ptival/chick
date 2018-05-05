{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.Label
  ( label_P
  ) where

import Text.Megaparsec.String

import Parsing.OCaml.Tokens

label_P :: Parser String
label_P = l_ident_T
