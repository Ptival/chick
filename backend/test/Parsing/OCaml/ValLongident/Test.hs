{-# LANGUAGE OverloadedStrings #-}

module Parsing.OCaml.ValLongident.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Parsing.OCaml.ValLongident
import Parsing.TestUtils

val_longident_tests :: [String]
val_longident_tests =
  [ "Foo.bar"
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.ValLongident" $ []
  ++ map (mkParsingTest "val_long_ident_P" val_longident_P) val_longident_tests

test :: IO ()
test = defaultMain unitTests
