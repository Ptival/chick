module Parsing.Unsafe
  ( unsafeParseInductive
  , unsafeParseScript
  , unsafeParseTerm
  ) where

import           Text.Megaparsec
import           Text.Printf

import           Inductive.Inductive
import           Parsing
import           Parsing.Inductive
import           Parsing.Script
import           Script
import           Term.Term
import qualified Term.Raw as Raw

unsafeParseTerm :: String -> Raw.Term Variable
unsafeParseTerm s =
  case parse termP "termP" s of
    Left  e -> error $ printf "unsafeParseTerm:\n%s\n%s" (errorBundlePretty e) s
    Right r -> r

unsafeParseInductive :: String -> Inductive Raw.Raw Variable
unsafeParseInductive s =
  case parse inductiveP "inductiveP" s of
    Left  e -> error $ printf "unsafeParseInductive:\n%s\n%s" (errorBundlePretty e) s
    Right r -> r

unsafeParseScript :: String -> Script Raw.Raw Variable
unsafeParseScript s =
  case parse scriptP "scriptP" s of
    Left  e -> error $ printf "unsafeParseScript:\n%s\n%s" (errorBundlePretty e) s
    Right r -> r
