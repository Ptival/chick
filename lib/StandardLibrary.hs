{-# language OverloadedStrings #-}

module StandardLibrary where

--import Data.Maybe

--import Notations
import Parsing
import Term.RawTerm
--import Term.Term
import Text.Printf

-- do not use `p` anywhere else!
unsafeParseRaw :: String -> RawTerm
unsafeParseRaw s =
  case parseMaybeTerm s of
    Nothing -> error $ printf "unsafeParseRaw: could not parse %s" s
    Just t  -> t

τId, tId :: RawTerm
τId = unsafeParseRaw "(T : Type) → T → T"
tId = unsafeParseRaw "λ _ x . x"

τFlip, tFlip :: RawTerm
τFlip = unsafeParseRaw
  "(A : Type) → (B : Type) → (C : Type) → (A → B → C) → (B → A → C)"
tFlip = unsafeParseRaw "λ _ _ _ f b a . f a b"

τNat :: RawTerm
τNat = unsafeParseRaw "(ℕ : Type) → (ℕ → ℕ) → ℕ → ℕ"

tZero, tOne, tTwo :: RawTerm
tZero = unsafeParseRaw "λ _ S O . O"
tOne  = unsafeParseRaw "λ _ S O . S O"
tTwo  = unsafeParseRaw "λ _ S O . S (S O)"

τNatBinOp :: RawTerm
τNatBinOp = unsafeParseRaw "(ℕ : Type) → ℕ → ℕ → (ℕ → ℕ) → ℕ → ℕ"

tSucc, tPlus, tMult :: RawTerm
tSucc = unsafeParseRaw "λ n S O . S (n S O)"
tPlus = unsafeParseRaw "λ m n S O . m S (n S O)"
tMult = unsafeParseRaw "λ m n S . m (n S)"

stdlib :: [(RawTerm, RawTerm)]
stdlib =
  [ (tId, τId)
  , (τFlip, tFlip)
  , (τNat, tZero)
  , (τNat, tOne)
  , (τNat, tTwo)
  , (τNatBinOp, tPlus)
  , (τNatBinOp, tMult)
  ]
