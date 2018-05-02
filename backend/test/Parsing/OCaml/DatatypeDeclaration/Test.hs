{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parsing.OCaml.DatatypeDeclaration.Test
  ( test
  , unitTests
  ) where

import Data.String.QQ
import Text.Megaparsec
import Test.Tasty

import OCaml
import Parsing.OCaml.DatatypeDeclaration

core_type_list_tests :: [String]
core_type_list_tests =
  [ "a * b"
  , ""
  , "a"
  , "*"
  ]

core_type_list_results :: [Maybe [Core_type]]
core_type_list_results = fmap (parseMaybe core_type_list_P) core_type_list_tests

simple_core_type2_tests :: [String]
simple_core_type2_tests =
  [ "'x"
  , "_"
  , "a"
  ]

simple_core_type2_results :: [Maybe Core_type]
simple_core_type2_results = fmap (parseMaybe simple_core_type2_P) simple_core_type2_tests

type_declaration_tests :: [String]
type_declaration_tests =
  [ "type a = _"
  , "type a = 'b"
  , "type a = b"
  , "type a = |"
  , "type a = A"
  , "type a = A of t"
  , "type a = A | B"
  , [s|
type stats_type =
  | Reporting
  | Driver
  |]
  ]

type_declaration_results :: [Maybe Type_declaration]
type_declaration_results = fmap (parseMaybe type_declaration_P) type_declaration_tests

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.DatatypeDeclaration" $ []
  -- ++ [testCase "and"   $ indAnd   @?= SL.indAnd  ]

test :: IO ()
test = defaultMain unitTests
