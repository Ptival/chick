module TestFresh where

import Test.Tasty
--import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

import Term.Free
import Term.Fresh
import Term.Raw as Raw

group :: String
group = "Fresh"

testFresh :: Raw.Term -> Bool
testFresh t = not (isFree (fresh t) t)

testFresh2 :: Raw.Term -> Raw.Term -> Bool
testFresh2 t1 t2 =
  let f = fresh2 t1 t2 in
  not (isFree f t1 || isFree f t2)

unitTests :: TestTree
unitTests =
  testGroup group $
  [
  ]

scTests :: TestTree
scTests =
  testGroup group $
  [ SC.testProperty "testFresh" testFresh
  , localOption (SmallCheckDepth 2) $
    SC.testProperty "testFresh2" testFresh2
  ]

qcTests :: TestTree
qcTests =
  testGroup group $
  [ QC.testProperty "testFresh" testFresh
  , QC.testProperty "testFresh2" testFresh2
  ]
