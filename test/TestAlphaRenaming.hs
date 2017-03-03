module TestAlphaRenaming where

import Test.Tasty
--import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC

import Term.AlphaEquivalence
import Term.AlphaRenaming
import Term.Free
import Term.RawTerm
import Term.Term

group :: String
group = "AlphaRenaming"

testRename :: Variable -> Variable -> RawTerm -> Bool
testRename a b t =
  if isFree a t
  then αrename a b t `αeq` t
  else αrename a b t == t

unitTests :: TestTree
unitTests =
  testGroup group $
  [
  ]

scTests :: TestTree
scTests =
  testGroup group $
  [ SC.testProperty "testRename" testRename
  ]

qcTests :: TestTree
qcTests =
  testGroup group $
  [ QC.testProperty "testRename" testRename
  ]
