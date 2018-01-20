{-# LANGUAGE FlexibleContexts #-}

module Inductive.Inductive.Test where

import           Test.Tasty

import           Examples.Utils
import           StandardLibrary
import           TestUtils

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
