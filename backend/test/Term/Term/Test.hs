{-# LANGUAGE FlexibleContexts #-}

module Term.Term.Test where

import           Test.Tasty

import           Examples.Term
import           Examples.Utils
import           TestUtils

equalityChecks :: [TestTree]
equalityChecks = [ equalityCheck t | t <- terms ]

inequalityChecks :: [TestTree]
inequalityChecks =
  [ inequalityCheck t1 t2 | (t1, t2) <- distinctPairs terms ]

unitTests :: TestTree
unitTests = testGroup "Term.Term" $ []
  ++ equalityChecks
  ++ inequalityChecks

test :: IO ()
test = defaultMain unitTests
