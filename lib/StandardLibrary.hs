{-# language OverloadedStrings #-}

module StandardLibrary where

import Data.Maybe

--import Notations
import Parsing
import Term.RawTerm
--import Term.Term

-- do not use `p` anywhere else!
unsafeParseRaw:: String -> RawTerm
unsafeParseRaw= fromJust . parseMaybeTerm

tId :: RawTerm
tId = unsafeParseRaw "λ x . x"

tFlip :: RawTerm
tFlip = unsafeParseRaw "λ f a b . f b a"

tZero, tOne, tTwo :: RawTerm
tZero = unsafeParseRaw "λ S O . O"
tOne  = unsafeParseRaw "λ S O . S O"
tTwo  = unsafeParseRaw "λ S O . S (S O)"

tSucc, tPlus, tMult :: RawTerm
tSucc = unsafeParseRaw "λ n S O . S (n S O)"
tPlus = unsafeParseRaw "λ m n S O . m S (n S O)"
tMult = unsafeParseRaw "λ m n S . m (n S)"

stdlib :: [RawTerm]
stdlib =
  [ tId
  , tFlip
  , tZero
  , tOne
  , tTwo
  , tPlus
  , tMult
  ]
