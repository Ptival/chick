{-# LANGUAGE OverloadedStrings #-}

module Parsing.OCaml.Expr.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Parsing.OCaml.Expr
import Parsing.OCaml.SeqExpr
import Parsing.TestUtils

expr_tests :: [String]
expr_tests =
  [ "function a -> b"
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.Expr" $ []
  ++ map (mkParsingTest "expr_P" (expr_P seq_expr_P)) expr_tests

test :: IO ()
test = defaultMain unitTests
