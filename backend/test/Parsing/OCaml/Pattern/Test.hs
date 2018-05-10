{-# LANGUAGE OverloadedStrings #-}

module Parsing.OCaml.Pattern.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Parsing.OCaml.Pattern
import Parsing.TestUtils

pattern_tests :: [String]
pattern_tests =
  [ "_"
  , "a"
  , "A"
  , "A a"
  , "A B"
  , "A _"
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.Pattern" $ []
  ++ map (mkParsingTest "pattern_P" pattern_P) pattern_tests

test :: IO ()
test = defaultMain unitTests
