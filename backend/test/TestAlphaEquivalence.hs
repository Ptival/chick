module TestAlphaEquivalence where

--import Test.Tasty.HUnit

import Term.AlphaEquivalence
import Term.Raw as Raw
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

group :: String
group = "AlphaEquivalence"

testReflexivity :: Raw.Term -> Bool
testReflexivity t = t `αeq` t

unitTests :: TestTree
unitTests =
  testGroup group $
    []

scTests :: TestTree
scTests =
  testGroup group $
    [ SC.testProperty "testReflexivity" testReflexivity
    ]

qcTests :: TestTree
qcTests =
  testGroup group $
    [ QC.testProperty "testReflexivity" testReflexivity
    ]
