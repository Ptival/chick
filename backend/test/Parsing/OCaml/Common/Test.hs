{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parsing.OCaml.Common.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Parsing.OCaml.Common
import Parsing.TestUtils

constr_ident_tests :: [String]
constr_ident_tests =
  [ "A"
  , "Ab"
  , "AB"
  , "[]"
  , "false"
  , "true"
  , "A "
  , "Ab "
  , "AB "
  , "[] "
  , "false "
  , "true "
  ]

ident_tests :: [String]
ident_tests =
  [ "A"
  , "Ab"
  , "AB"
  , "a"
  , "aB"
  , "ab"
  , "A "
  , "Ab "
  , "AB "
  , "a "
  , "aB "
  , "ab "
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.DatatypeDeclaration" $ []
  ++ map (mkParsingTest "constr_ident_P" constr_ident_P) constr_ident_tests
  ++ map (mkParsingTest "ident_P" ident_P) ident_tests

test :: IO ()
test = defaultMain unitTests
