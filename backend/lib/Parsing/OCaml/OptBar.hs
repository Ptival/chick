{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.OptBar
  ( opt_bar_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import Parsing.OCaml.Tokens

opt_bar_P :: Parser ()
opt_bar_P = choice
  [ bar_T *> return ()
  , return ()
  ]
