{-# language OverloadedStrings #-}

module StandardLibrary where

--import Data.Maybe

--import Notations
import Parsing
import Term.Raw         as Raw
--import Term.Term
import Term.TypeChecked as TypeChecked
import Text.Printf

-- do not use `unsafeParseRaw` anywhere else!
unsafeParseRaw :: String -> Raw.Term
unsafeParseRaw s =
  case parseMaybeTerm s of
    Nothing -> error $ printf "unsafeParseRaw: could not parse %s" s
    Just t  -> t

τId, tId :: Raw.Term
τId = unsafeParseRaw "(T : Type) → T → T"
tId = unsafeParseRaw "λ T x . x"

τFlip, tFlip :: Raw.Term
τFlip = unsafeParseRaw
  "(A B C : Type) → (A → B → C) → (B → A → C)"
tFlip = unsafeParseRaw "λ A B C f b a . f a b"

nat :: String
nat = "((T → T) → T → T)"

τNat :: Raw.Term
τNat = unsafeParseRaw $ printf "(T : Type) → %s" nat

tZero, tOne, tTwo :: Raw.Term
tZero = unsafeParseRaw "λ T S O . O"
tOne  = unsafeParseRaw "λ T S O . S O"
tTwo  = unsafeParseRaw "λ T S O . S (S O)"

τNatUnOp , τNatBinOp :: Raw.Term
τNatUnOp  = unsafeParseRaw $ printf "(T : Type) → %s → %s"      nat nat
τNatBinOp = unsafeParseRaw $ printf "(T : Type) → %s → %s → %s" nat nat nat

tSucc, tPlus, tMult :: Raw.Term
tSucc = unsafeParseRaw "λ T n S O . S (n S O)"
tPlus = unsafeParseRaw "λ T m n S O . m S (n S O)"
tMult = unsafeParseRaw "λ T m n S . m (n S)"

axioms :: [(TypeChecked.Term, TypeChecked.Term)]
axioms =
  [ -- (Var (Type ()) "T", Type ())
  ]

stdlib :: [(Raw.Term, Raw.Term)]
stdlib =
  [ (tId, τId)
  , (τFlip, tFlip)
  , (τNat, tZero)
  , (τNat, tOne)
  , (τNat, tTwo)
  , (τNatBinOp, tPlus)
  , (τNatBinOp, tMult)
  ]
