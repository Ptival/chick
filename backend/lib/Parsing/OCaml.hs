{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.DatatypeDeclaration
import Parsing.OCaml.Common
import Parsing.OCaml.Tokens
-- import Parsing.Utils

{-

Note: to convert a left-recursive grammar

A:
| B
| A C

into a non-left-recursive grammar, do:

A:
| B R

R:
| C R
| ε

for the productions, use a continuation-style where R returns a result transformer

-}
