{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.ModExtLongident
  ( mod_ext_longident_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Tokens
import Parsing.Utils

mod_ext_longident_P :: Parser Longident
mod_ext_longident_P = leftRecursive
  [ Lident <$> u_ident_T
  ]
  [ try $ do
    dot_T
    i <- u_ident_T
    return $ \ x -> Ldot x i
    -- TODO: parens
  ]
