{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.ModExtLongident
  ( mod_ext_longident_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.Tokens

mod_ext_longident_P :: Parser Longident
mod_ext_longident_P = try $ do
  a <- Lident <$> u_ident_T
  b <- rest
  return $ b a
  where
    rest = choice
      [ try $ do
        dot_T
        i <- u_ident_T
        r <- rest
        return $ \ x -> r $ Ldot x i
      -- TODO: parens
      , return id
      ]
