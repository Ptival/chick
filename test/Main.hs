{-# language OverloadedStrings #-}

module Main where

import           Data.Default
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC
import           Text.Megaparsec
import           Text.Printf

import           Notations
import           Parsing
import           Precedence
import           PrettyPrinting
import           Term.RawTerm
import           Term.Term
import qualified TestFresh             as TF
import qualified TestAlphaEquivalence  as TAE
import qualified TestAlphaRenaming     as TAR
import           WellFormed

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Tests"
  [ testGroup "(checked by HUnit)" $
    [ unitTests
    , TAE.unitTests
    , TAR.unitTests
    , TF.unitTests
    ]

  , localOption (SmallCheckDepth 3) $
    testGroup "(checked by SmallCheck)" $
    [ TAE.scTests
    , TAR.scTests
    , TF.scTests
    ]

  , localOption (QuickCheckMaxSize 30) $
    localOption (QuickCheckTests 1000) $
    localOption (QuickCheckReplay Nothing) $
    localOption (QuickCheckShowReplay True) $
    --localOption (QuickCheckVerbose True) $
    testGroup "(checked by QuickCheck)" $
    [ TAE.qcTests
    , TAR.qcTests
    , TF.qcTests
    ]

  ]

parseMaybeRaw :: String -> Maybe RawTerm
parseMaybeRaw = parseMaybe termP

prettyRaw :: RawTerm -> String
prettyRaw = prettyTermString def

unitTestTerms :: [(RawTerm, String)]
unitTestTerms =
  [ (var "a" ^:: var "b", printf "a %s b" annotSymbol)
  , (var "a" ^$ var "b", "a b")
  , (hole, holeSymbol)
  , ((^\) ["arg1", "arg2", "arg3"] (var "body"), "λ arg1 arg2 arg3 . body")
  , ( let' [("x1", var "body1"), ("x2", var "body2")] (var "body")
    , "let x1 = body1 in let x2 = body2 in body"
    )
  , (π [("t", var "τ1")] (var "τ2"), "(t : τ1) → τ2")
  , (var "τ1" ^-> var "τ2", "τ1 → τ2")
  , (type', "Type")
  , (var "foo", "foo")
  ]

unitTests :: TestTree
unitTests =

  let isTolerable' = isTolerable (tableToOrdering def) in
  let isNotTolerable' c p = not (isTolerable' c p) in

  testGroup "Unit tests" $

  [ testCase ("parsing " ++ s) $ parseMaybeRaw s @?= Just t | (t, s) <- unitTestTerms ]

  ++

  [ testCase ("printing " ++ s) $ prettyRaw t @?= s | (t, s) <- unitTestTerms ]

  ++

  [ let name = "isNotTolerable' PrecAnnot (PrecApp, TolerateHigher)" in
      testCase name $
      isNotTolerable' PrecAnnot (PrecApp, TolerateHigher) @? ":("

  , let name = "isNotTolerable' PrecAnnot (PrecApp, TolerateEqual)" in
      testCase name $
      isNotTolerable' PrecAnnot (PrecApp, TolerateEqual) @? ":("

  ]

  ++

  [ let bad = printf "a %s b %s c" annotSymbol annotSymbol in
    testCase (printf "should not parse %s" bad) $
    parseMaybeRaw bad @?= Nothing ]

scProps :: TestTree
scProps =

  let isTolerable' = isTolerable (tableToOrdering def) in

  localOption (SmallCheckDepth 1) $

  testGroup "(checked by SmallCheck)" $

  [ SC.testProperty
    "isTolerable' p (q, TolerateAny)" $
    \ p q -> isTolerable' p (q, TolerateAny)

  , SC.testProperty
    "isTolerable' p (p, TolerateEqual)" $
    \ p -> isTolerable' p (p, TolerateEqual)

  , SC.testProperty
    "not isTolerable' p (p, TolerateHigher)" $
    \ p -> not $ isTolerable' p (p, TolerateHigher)

  , SC.testProperty
    "parseMaybeRaw . prettyRaw == Just" $
    \ t -> wellFormed t SC.==> (parseMaybeRaw . prettyRaw) t == Just t

  ]

qcProps :: TestTree
qcProps =

  testGroup "Something something roundtrip" $

  [ QC.testProperty
    "parseMaybeRaw . prettyRaw == Just" $
    \ t -> wellFormed t QC.==> (parseMaybeRaw . prettyRaw) t == Just t

  ]
