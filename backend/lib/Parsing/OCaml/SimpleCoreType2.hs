{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.SimpleCoreType2
  ( simple_core_type2_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Common
import Parsing.OCaml.Tokens
import Parsing.OCaml.TypeLongident

simple_core_type2_P :: Parser Core_type
simple_core_type2_P = try $ do
  a <- choice
       [ mkTyp . Ptyp_var <$> try (quote_T *> ident_P)
       , mkTyp . const Ptyp_any <$> underscore_T
       , try $ do
         t <- type_longident_P
         return $ mkTyp $ Ptyp_constr (mkRHS t 1) []
       -- , do
       --   a <- chainl1' simple_core_type2_P _ _ -- (return $ \ a b -> Ptyp_constr (mkRHS b 2) [a])
       --   t <- type_longident_P
       --   return $ mkTyp $ Ptyp_constr (mkRHS t 2) [a]
       ]
  b <- rest
  return $ b a
  where
    rest = choice
      [ do
        t <- type_longident_P
        r <- rest
        return $ \ x -> r . mkTyp $ Ptyp_constr (mkRHS t 2) [x]
      , return id
      ]
