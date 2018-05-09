{-# LANGUAGE OverloadedStrings #-}

module Parsing.OCaml.ConstrLongident.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Parsing.OCaml.ConstrLongident
import Parsing.TestUtils

constr_longident_tests :: [String]
constr_longident_tests =
  [ "Foo"
  , "Foo.Bar"
  , "Foo_foo.Bar_bar"
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.ConstrLongident" $ []
  ++ map (mkParsingTest "constr_longident_P" constr_longident_P) constr_longident_tests

test :: IO ()
test = defaultMain unitTests
