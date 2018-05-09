{-# LANGUAGE OverloadedStrings #-}

module Parsing.OCaml.MatchCase.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Parsing.OCaml.Pattern
import Parsing.TestUtils

pattern_tests :: [String]
pattern_tests =
  [ "a"
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.MatchCase" $ []
  ++ map (mkParsingTest "pattern_P" pattern_P) pattern_tests

test :: IO ()
test = defaultMain unitTests
