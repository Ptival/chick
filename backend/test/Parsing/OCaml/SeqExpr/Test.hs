{-# LANGUAGE OverloadedStrings #-}

module Parsing.OCaml.SeqExpr.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Parsing.OCaml.SeqExpr
import Parsing.TestUtils

seq_expr_tests :: [String]
seq_expr_tests =
  [ "b"
  , "Foo.bar"
  , "Foo.bar_baz"
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.SeqExpr" $ []
  ++ map (mkParsingTest "seq_expr_P" seq_expr_P) seq_expr_tests

test :: IO ()
test = defaultMain unitTests
