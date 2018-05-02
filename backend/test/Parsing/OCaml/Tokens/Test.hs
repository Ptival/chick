{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parsing.OCaml.Tokens.Test
  ( test
  , unitTests
  ) where

import Data.String.QQ
import Text.Megaparsec
import Test.Tasty

import OCaml
import Parsing.OCaml.Tokens

l_ident_tests :: [String]
l_ident_tests = [ "a", "a " ]

l_ident_results :: [Maybe String]
l_ident_results = fmap (parseMaybe l_ident_T) l_ident_tests

u_ident_tests :: [String]
u_ident_tests = [ "A", "A " ]

u_ident_results :: [Maybe String]
u_ident_results = fmap (parseMaybe l_ident_T) l_ident_tests

star_tests :: [String]
star_tests = [ "*", "* ", " *", " * " ]

star_results :: [Maybe ()]
star_results = fmap (parseMaybe star_T) star_tests

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.DatatypeDeclaration" $ []
  -- ++ [testCase "and"   $ indAnd   @?= SL.indAnd  ]

test :: IO ()
test = defaultMain unitTests
