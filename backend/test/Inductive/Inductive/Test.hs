{-# LANGUAGE FlexibleContexts #-}

module Inductive.Inductive.Test where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf

import           Inductive.Inductive
import           StandardLibrary
import           Term.Term
import qualified Term.Raw as Raw

equalityChecks :: [TestTree]
equalityChecks =
  [ equalityCheck ind | ind <- inductives ]

inequalityChecks :: [TestTree]
inequalityChecks =
  [ inequalityCheck ind1 ind2 | (ind1, ind2) <- distinctPairs inductives ]

unitTests :: TestTree
unitTests = testGroup "Inductive.Inductive" $ []
  ++ equalityChecks
  ++ inequalityChecks

test :: IO ()
test = defaultMain unitTests
