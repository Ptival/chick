{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.Constant
  ( constant_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Tokens

constant_P :: Parser Constant
constant_P = choice
  [ do
    (n, m) <- int_T
    return $ Pconst_integer n m
  -- , Pconst_char <$> char_T
  -- , do
  --   (s, d) <- string_T
  --   return $ Pconst_string s d
  -- , do
  --   (f, m) <- string_T
  --   return $ Pconst_float f m
  ]
