{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.GeneralizedConstructorArguments
  ( generalized_constructor_arguments_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.ConstructorArguments
import Parsing.OCaml.Tokens

generalized_constructor_arguments_P :: Parser (Constructor_arguments, Maybe a)
generalized_constructor_arguments_P = choice
  [ flip (,) Nothing <$> (of_T *> constructor_arguments_P)
    -- TODO: colon
  , return (Pcstr_tuple [], Nothing)
  ]
