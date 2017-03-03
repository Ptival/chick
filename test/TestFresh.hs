module TestFresh where

import Test.Tasty
--import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

import Term.Free
import Term.Fresh
import Term.RawTerm

group :: String
group = "Substitution"

testFresh :: RawTerm -> Bool
testFresh t = not (isFree (fresh t) t)

testFresh2 :: RawTerm -> RawTerm -> Bool
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
  , SC.testProperty "testFresh2" testFresh2
  ]

qcTests :: TestTree
qcTests =
  testGroup group $
  [ QC.testProperty "testFresh" testFresh
  , QC.testProperty "testFresh2" testFresh2
  ]
