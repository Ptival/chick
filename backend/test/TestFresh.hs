module TestFresh where

--import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC

import Term.Free
import Term.Fresh
import qualified Term.Raw as Raw
import Term.Term
import Test.Tasty

group :: String
group = "Fresh"

testFresh :: Raw.Term Variable -> Bool
testFresh t = not (isFree (fresh t) t)

testFresh2 :: Raw.Term Variable -> Raw.Term Variable -> Bool
testFresh2 t1 t2 =
  let f = fresh2 t1 t2
   in not (isFree f t1 || isFree f t2)

unitTests :: TestTree
unitTests =
  testGroup group $
    []

scTests :: TestTree
scTests =
  testGroup group $ []

-- ++ [ SC.testProperty "testFresh" testFresh ]
-- ++ [ localOption (SmallCheckDepth 2) $
--      SC.testProperty "testFresh2" testFresh2
--    ]

qcTests :: TestTree
qcTests =
  testGroup group $ []

-- ++ [ QC.testProperty "testFresh" testFresh ]
-- ++ [ QC.testProperty "testFresh2" testFresh2 ]
