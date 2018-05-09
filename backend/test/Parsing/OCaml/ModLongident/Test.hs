{-# LANGUAGE OverloadedStrings #-}

module Parsing.OCaml.ModLongident.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Parsing.OCaml.ModLongident
import Parsing.TestUtils

mod_longident_tests :: [String]
mod_longident_tests =
  [ "Foo"
  , "Foo.Bar"
  , "Foo_foo.Bar_bar"
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.ModLongident" $ []
  ++ map (mkParsingTest "mod_longident_P" mod_longident_P) mod_longident_tests

test :: IO ()
test = defaultMain unitTests
