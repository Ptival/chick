{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.OpenStatement
  ( open_statement_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.ModLongident
import Parsing.OCaml.Override_flag
import Parsing.OCaml.PostItemAttributes
import Parsing.OCaml.Tokens

open_statement_P :: Parser Structure -> Parser (Open_description, ())
open_statement_P structure_P = do
  try $ open_T
  o <- override_flag_P
  -- TODO: ext_attributes
  i <- mod_longident_P
  a <- post_item_attributes_P structure_P
  return $ (
    mkOpn
    Nothing -- FIXME
    Nothing -- FIXME
    Nothing -- FIXME
    (Just o)
    (mkRHS i 4)
    , ())
