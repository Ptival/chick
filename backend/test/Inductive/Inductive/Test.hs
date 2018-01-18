{-# LANGUAGE FlexibleContexts #-}

module Inductive.Inductive.Test where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf

import           Inductive.Inductive
import           StandardLibrary
import           Term.Term
import qualified Term.Raw as Raw

{- generates all pairs of distinct inductives -}
inductiveCombinations :: [(Inductive Raw.Raw Variable, Inductive Raw.Raw Variable)]
inductiveCombinations =
  let maxIndex = length inductives - 1 in
  [ (inductives !! i1, inductives !! i2)
  | i1 <- [0 .. maxIndex], i2 <- [i1 + 1 .. maxIndex]
  ]

equalityCheck :: (Show a, Eq (Inductive α a)) => Inductive α a -> TestTree
equalityCheck ind =
  let name = show $ inductiveName ind in
  testCase
  (printf "%s == %s" name name)
  $ (ind == ind) @? "inductive did not equate itself"

equalityChecks :: [TestTree]
equalityChecks =
  [ equalityCheck ind | ind <- inductives ]

inequalityCheck :: (Eq (Inductive α a), Show a) => Inductive α a -> Inductive α a -> TestTree
inequalityCheck ind1 ind2 =
  testCase
  (printf "%s /= %s" (show $ inductiveName ind1) (show $ inductiveName ind2))
  $ (ind1 /= ind2) @? "distinct inductives equated each other"

inequalityChecks :: [TestTree]
inequalityChecks =
  [ inequalityCheck ind1 ind2 | (ind1, ind2) <- inductiveCombinations ]

unitTests :: TestTree
unitTests = testGroup "Inductive.Inductive" $ []
  ++ equalityChecks
  ++ inequalityChecks

test :: IO ()
test = defaultMain unitTests

failing :: IO ()
failing =
  defaultMain $ testGroup "Inductive.Inductive" [ inequalityCheck indNat indUnit ]
