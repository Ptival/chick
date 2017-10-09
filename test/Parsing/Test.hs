module Parsing.Test
  ( main
  , unitTests
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
    ++ [ testCase "empty match" $ testParses "match foo with end" @? "empty match" ]
    ++ [ testCase "match bool"
       $ testParses "match b with | true => x | false => y end"
       @? "match bool"
       ]
    ++ [ testCase "match list"
       $ testParses "match l with | nil _ => O | cons _ h t => S _ end"
       @? "match list"
       ]

main :: IO ()
main = defaultMain unitTests
