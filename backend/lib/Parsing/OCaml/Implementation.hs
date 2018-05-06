{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.Implementation
  ( implementation_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree
import Parsing.OCaml.Structure
import Parsing.OCaml.Utils

implementation_P :: Parser Structure
implementation_P = ocamlSpace *> structure_P <* eof
