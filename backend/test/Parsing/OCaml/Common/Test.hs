{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parsing.OCaml.Common.Test
  ( test
  , unitTests
  ) where

import Data.String.QQ
import Text.Megaparsec
import Test.Tasty

import OCaml
import Parsing.OCaml.Common

constr_ident_tests :: [String]
constr_ident_tests =
  [ "A"
  , "Ab"
  , "AB"
  , "[]"
  , "false"
  , "true"
  ]

constr_ident_results :: [Maybe String]
constr_ident_results = fmap (parseMaybe constr_ident_P) constr_ident_tests

ident_tests :: [String]
ident_tests =
  [ "A"
  , "Ab"
  , "AB"
  , "a"
  , "aB"
  , "ab"
  ]

ident_results :: [Maybe String]
ident_results = fmap (parseMaybe ident_P) ident_tests

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.DatatypeDeclaration" $ []
  -- ++ [testCase "and"   $ indAnd   @?= SL.indAnd  ]

test :: IO ()
test = defaultMain unitTests
