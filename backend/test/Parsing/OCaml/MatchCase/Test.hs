{-# LANGUAGE OverloadedStrings #-}

module Parsing.OCaml.MatchCase.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Parsing.OCaml.MatchCase
import Parsing.OCaml.SeqExpr
import Parsing.TestUtils

match_case_tests :: [String]
match_case_tests =
  [ "a -> b"
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.MatchCase" $ []
  ++ map (mkParsingTest "match_case_P" (match_case_P seq_expr_P)) match_case_tests

test :: IO ()
test = defaultMain unitTests
