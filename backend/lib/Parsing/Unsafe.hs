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
  case parseMaybeTerm s of
    Nothing -> error $ printf "unsafeParseTerm: could not parse %s" s
    Just t  -> t

unsafeParseInductive :: String -> Inductive Raw.Raw Variable
unsafeParseInductive s =
  case parseMaybe inductiveP s of
    Nothing -> error $ printf "unsafeParseInductive: could not parse\n%s" s
    Just t  -> t

unsafeParseScript :: String -> Script Raw.Raw Variable
unsafeParseScript s =
  case parseMaybe scriptP s of
    Nothing -> error $ printf "unsafeParseInductive: could not parse\n%s" s
    Just t  -> t
