{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.ConstructorDeclaration
  ( constructor_declaration_P
  ) where

import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.Common
import Parsing.OCaml.GeneralizedConstructorArguments

constructor_declaration_P :: Parser Constructor_declaration
constructor_declaration_P = do
  name <- constr_ident_P
  (args, res) <- generalized_constructor_arguments_P
  -- attributes_P
  return $ constructor args res (mkRHS name 1)
