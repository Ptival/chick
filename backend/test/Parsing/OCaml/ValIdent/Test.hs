{-# LANGUAGE OverloadedStrings #-}

module Parsing.OCaml.ValIdent.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Parsing.OCaml.ValIdent
import Parsing.TestUtils

val_ident_tests :: [String]
val_ident_tests =
  [ "foo"
  , "(!)"
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.ValIdent" $ []
  ++ map (mkParsingTest "val_ident_P" val_ident_P) val_ident_tests

test :: IO ()
test = defaultMain unitTests
