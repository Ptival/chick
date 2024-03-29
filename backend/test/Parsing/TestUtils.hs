module Parsing.TestUtils
  ( debugParsing,
    mkParsingTest,
    mkParsingTestFromFile,
    parse,
  )
where

import Data.Maybe
import Data.Void
import Parsing.Types
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

mkParsingTest :: TestName -> Parser a -> String -> TestTree
mkParsingTest name parser input =
  let prefix = take 20 input
   in testCase name $
        isJust (parseMaybe parser input)
          @? "Failed to parse:\n" ++ prefix ++ if length input > 20 then "..." else ""

mkParsingTestFromFile :: Parser a -> FilePath -> TestTree
mkParsingTestFromFile parser fileName =
  testCase fileName $
    ( do
        input <- readFile fileName
        return $ isJust (parseMaybe parser input)
    )
      @? "Failed to parse " ++ fileName

debugParsing :: Parser a -> String -> Either (ParseError (Token String) Void) a
debugParsing parser input =
  parse (parser <* eof) "DEBUG" input
