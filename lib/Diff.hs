{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Diff where

import Data.Generic.Diff

import Tactic
--import Term.AlphaEquivalence
import Term.Diff
import Term.Term
--import Term.TypeChecked   as TypeChecked
import Term.Raw           as Raw
import Typing.TypeChecker
import StandardLibrary

tactic0 :: Tactic
tactic0 =
  Semicolon (Atomic (Intro "A")) $
  Semicolon (Atomic (Intro "a")) $
  Atomic (Exact "a")

main :: IO ()
main = do
  let ttype = unsafeParseRaw "Type"
  let checkType t = typeCheck [] (unsafeParseRaw t) ttype
  let Right concl0 = checkType "(A : Type) → A → A"
  let Right concl1 = checkType "(B : Type) → (A : Type) → A → A"
  case runTactic [] tactic0 (Goal [] concl0) of
    Left  _ -> putStrLn "Tactic did not solve the original goal"
    Right _ -> do
      let conclDiff = diff (raw concl0) (raw concl1) :: TermDiff Raw
      case patchTactic concl0 tactic0 conclDiff of
        Nothing -> putStrLn "Could not patch the tactic"
        Just tactic1 -> do
          case runTactic [] tactic1 (Goal [] concl1) of
            Left  _ -> do
              putStrLn "Patched tactic did not solve modified goal"
              putStrLn "Patched tactic:"
              putStrLn $ show tactic1
            Right _ -> putStrLn "SUCCESS!"

aa, bb, ab :: Raw.Term
aa = unsafeParseRaw "(a : T) → a a"
bb = unsafeParseRaw "(b : T) → b b"
ab = unsafeParseRaw "(b : T) → a b"

τ1, τ2, τ3 :: Raw.Term
τ1 = unsafeParseRaw "T → T"
τ2 = unsafeParseRaw "U → T → T"
τ3 = unsafeParseRaw "T → U → T"

diff12, diff13, diff23 :: TermDiff Raw
diff12 = diff τ1 τ2
diff13 = diff τ1 τ3
diff23 = diff τ2 τ3

patchAtomic :: TermX ξ -> Atomic -> TermDiff ψ -> Maybe Atomic
patchAtomic _τ atomic = \case
  _ -> Just atomic

patchTactic :: TermX ξ -> Tactic -> TermDiff ψ -> Maybe Tactic
patchTactic _τ tactic = \case
  _ -> Just tactic
