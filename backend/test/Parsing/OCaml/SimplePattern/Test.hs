{-# LANGUAGE OverloadedStrings #-}

module Parsing.OCaml.SimplePattern.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Parsing.OCaml.SimplePattern
import Parsing.TestUtils

simple_pattern_tests :: [String]
simple_pattern_tests =
  [ "_"
  , "a"
  , "A"
  , "A.B"
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.Pattern" $ []
  ++ map (mkParsingTest "simple_pattern_P" simple_pattern_P) simple_pattern_tests

test :: IO ()
test = defaultMain unitTests
