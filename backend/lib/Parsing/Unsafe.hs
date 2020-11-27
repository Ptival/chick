module Parsing.Unsafe
  ( unsafeParseInductive,
    unsafeParseScript,
    unsafeParseTerm,
  )
where

import Inductive.Inductive (Inductive)
import Parsing (termP)
import Parsing.Inductive (inductiveP)
import Parsing.Script (scriptP)
import Script (Script)
import qualified Term.Raw as Raw
import Term.Term (Variable)
import Text.Megaparsec (errorBundlePretty, parse)
import Text.Printf (printf)

unsafeParseTerm :: String -> Raw.Term Variable
unsafeParseTerm s =
  case parse termP "termP" s of
    Left e -> error $ printf "unsafeParseTerm:\n%s\n%s" (errorBundlePretty e) s
    Right r -> r

unsafeParseInductive :: String -> Inductive Raw.Raw Variable
unsafeParseInductive s =
  case parse inductiveP "inductiveP" s of
    Left e -> error $ printf "unsafeParseInductive:\n%s\n%s" (errorBundlePretty e) s
    Right r -> r

unsafeParseScript :: String -> Script Raw.Raw Variable
unsafeParseScript s =
  case parse scriptP "scriptP" s of
    Left e -> error $ printf "unsafeParseScript:\n%s\n%s" (errorBundlePretty e) s
    Right r -> r
