{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.Override_flag
  ( override_flag_P
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.String

import qualified OCaml.Parsing.ASTTypes as ASTTypes
import           Parsing.OCaml.Tokens

override_flag_P :: Parser ASTTypes.Override_flag
override_flag_P = choice
  [ bang_T *> return ASTTypes.Override
  , return ASTTypes.Fresh
  ]
