{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parsing.OCaml.Structure.Test
  ( test
  , unitTests
  ) where

import Data.String.QQ
import Test.Tasty

import Parsing.OCaml.Structure
import Parsing.TestUtils

structure_tests :: [String]
structure_tests =
  [ " "
  , "type a = A"
  , "type a = _"
  , "type a = b"
  , "type a = 'b"
  , "type a = _ ;; type a = b ;; type a = 'b"
  , [s|
type a = _
type a = 'b
type a = b
type a = |
type a = A
type a = A of t
type a = A of t | B of t
type a = A | B
type a = A | B | C
type a = | A | B | C
|]
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.DatatypeDeclaration" $ []
  ++ map (mkParsingTest "structure_item_P" structure_P) structure_tests

test :: IO ()
test = defaultMain unitTests
