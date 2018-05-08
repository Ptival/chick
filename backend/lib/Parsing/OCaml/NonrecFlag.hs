{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.NonrecFlag
  ( nonrec_flag_P
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.String

import qualified OCaml.Parsing.ASTTypes as ASTTypes
import           Parsing.OCaml.Tokens

nonrec_flag_P :: Parser ASTTypes.Rec_flag
nonrec_flag_P = choice
  [ nonrec_T *> return ASTTypes.Nonrecursive
  , return ASTTypes.Recursive
  ]
