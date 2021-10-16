module Inductive.Inductive.Test where

import Examples.Utils
import Language
import PrettyPrinting.Chick ()
import StandardLibrary
import Test.Tasty
import TestUtils

equalityChecks :: [TestTree]
equalityChecks =
  [equalityCheck @'Chick ind | ind <- inductives]

inequalityChecks :: [TestTree]
inequalityChecks =
  [inequalityCheck @'Chick ind1 ind2 | (ind1, ind2) <- distinctPairs inductives]

unitTests :: TestTree
unitTests =
  testGroup "Inductive.Inductive" $
    []
      ++ equalityChecks
      ++ inequalityChecks

test :: IO ()
test = defaultMain unitTests
