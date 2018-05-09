{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.ConstrLongident
  ( constr_longident_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.ModLongident
import Parsing.OCaml.Tokens
import Parsing.OCaml.Utils

constr_longident_P :: Parser Longident
constr_longident_P = choice
  [ do
    i <- try $ do
      i <- mod_longident_P
      dot_T
      return i
    parens colon_colon_T
    return $ Ldot i "::"
  , mod_longident_P
  , l_bracket_T *> r_bracket_T *> (return $ Lident "[]")
  , l_paren_T *> r_paren_T *> (return $ Lident "()")
  , l_paren_T *> colon_colon_T *> r_paren_T *> (return $ Lident "::")
  , false_T *> (return $ Lident "false")
  , true_T *> (return $ Lident "true")
  ]
