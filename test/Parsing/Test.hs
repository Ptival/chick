module Parsing.Test
  ( unitTests
  ) where

import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit

import Parsing

group :: String
group = "Parsing"

testParses :: String -> Bool
testParses = isJust . parseMaybeTerm

unitTests :: TestTree
unitTests =
  testGroup group $ []
    ++ [ testCase "variable"    $ testParses "foo" @? "variable" ]
    ++ [ testCase "application" $ testParses "a b" @? "application" ]
    ++ [ testCase "parentheses" $ testParses "a (b (c d) e) (f g) (h)" @? "parentheses" ]
    ++ [ testCase "lambda"      $ testParses "λ f _ x , f x" @? "lambda" ]
