{-# LANGUAGE OverloadedStrings #-}

module Parsing.OCaml.OpenStatement.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Parsing.OCaml.OpenStatement
import Parsing.OCaml.Structure
import Parsing.TestUtils

open_statement_tests :: [String]
open_statement_tests =
  [ "open A"
  , "open !A"
  -- , "(* A *) open !A"
  , "open !A (* A *)"
  -- , "(* A *) open !A (* A *)"
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.OpenStatement" $ []
  ++ map (mkParsingTest "open_statement_P" (open_statement_P structure_P)) open_statement_tests

test :: IO ()
test = defaultMain unitTests
