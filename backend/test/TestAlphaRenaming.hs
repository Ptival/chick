module TestAlphaRenaming where

import Parsing
--import PrettyPrinting
import Term.AlphaEquivalence
import Term.AlphaRenaming
import Term.Free
import Term.Fresh
import Term.Raw as Raw
import Term.Term
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Text.Printf

group :: String
group = "AlphaRenaming"

testRename :: Variable -> Raw.Term -> Bool
testRename r t =
  let f = fresh t
   in let t' = αrename f r t
       in t' `αeq` t

bindVars :: [Variable] -> Raw.Term -> Raw.Term
  bindVars vs t = foldr (Lam () . Binder . Just) t vs

bindFreeVars :: Raw.Term alpha -> Raw.Term alpha
bindFreeVars t = bindVars (freeVars t) t

unitTests :: TestTree
unitTests =
  let matchRename s1 a b s2 =
        testCase (printf "matchRename %s [ %s ← %s ] is %s" s1 a b s2) $
          case (parseMaybeTerm s1, parseMaybeTerm s2) of
            (Just t1, Just t2) ->
              αrename (Variable a) (Variable b) t1 `αeq` t2
                @? "α-renamed term is not α-equivalent to the expectation"
            _ -> False @? "terms to be tested did not parse"
   in testGroup group $
        [ matchRename "a b" "a" "b" "b b",
          matchRename "a b" "b" "a" "a a",
          matchRename "a b" "c" "a" "a b",
          matchRename "a b" "c" "b" "a b",
          matchRename "a (λ a . a)" "a" "b" "b (λ a . a)",
          matchRename "a (λ a . b)" "a" "b" "b (λ a . b)",
          matchRename "a (λ b . b)" "a" "b" "b (λ b . b)",
          matchRename "a (λ b . a)" "a" "b" "b (λ c . b)"
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
