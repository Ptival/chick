module Term.Term.Test where

import Examples.Term
import Examples.Utils
import Language
import Test.Tasty
import TestUtils

equalityChecks :: [TestTree]
equalityChecks = [equalityCheck @'Chick t | t <- terms]

inequalityChecks :: [TestTree]
inequalityChecks =
  [inequalityCheck @'Chick t1 t2 | (t1, t2) <- distinctPairs terms]

unitTests :: TestTree
unitTests =
  testGroup "Term.Term" $
    []
      ++ equalityChecks
      ++ inequalityChecks

test :: IO ()
test = defaultMain unitTests
