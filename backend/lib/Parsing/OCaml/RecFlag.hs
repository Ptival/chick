{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.RecFlag
  ( rec_flag_P
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.String

import qualified OCaml.Parsing.ASTTypes as ASTTypes
import           Parsing.OCaml.Tokens

rec_flag_P :: Parser ASTTypes.Rec_flag
rec_flag_P = choice
  [ rec_T *> return ASTTypes.Recursive
  , return ASTTypes.Nonrecursive
  ]
