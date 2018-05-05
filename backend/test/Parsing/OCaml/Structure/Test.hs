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

prefix :: FilePath
prefix = "test/Parsing/OCaml/Structure/"

files :: [FilePath]
files = map (prefix ++)
  [ "infer_00.ml"
  , "infer_01.ml"
  , "infer_02.ml"
  , "infer_03.ml"
  , "infer_04.ml"
  , "infer_05.ml"
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.DatatypeDeclaration" $ []
  ++ map (mkParsingTest "structure_P" structure_P) structure_tests
  ++ map (mkParsingTestFromFile "structure_P" structure_P) files

test :: IO ()
test = defaultMain unitTests
