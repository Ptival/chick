module Parsing.TestUtils
  ( mkParsingTest
  , parse
  ) where

import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.String
import Test.Tasty
import Test.Tasty.HUnit

mkParsingTest :: TestName -> Parser a -> String -> TestTree
mkParsingTest name parser input =
  let prefix = take 20 input in
  testCase name
  $ isJust (parseMaybe parser input)
  @? "Failed to parse:\n" ++ prefix ++ if length input > 20 then "..." else ""
