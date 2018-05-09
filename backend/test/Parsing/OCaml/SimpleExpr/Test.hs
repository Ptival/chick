{-# LANGUAGE OverloadedStrings #-}

module Parsing.OCaml.SimpleExpr.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Parsing.OCaml.SimpleExpr
import Parsing.TestUtils

simple_expr_tests :: [String]
simple_expr_tests =
  [ "foo"
  , "foo.bar"
  , "Foo"
  , "Foo.Bar"
  , "Foo.bar"
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.SimpleExpr" $ []
  ++ map (mkParsingTest "simple_expr_P" simple_expr_P) simple_expr_tests

test :: IO ()
test = defaultMain unitTests
