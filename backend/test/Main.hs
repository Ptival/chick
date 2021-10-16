{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Default
-- import qualified Diff.ConcatMap.Test
-- import qualified Diff.Eliminator.Test
-- import qualified Diff.Guess.Constructor.Test
-- import qualified Diff.Guess.Inductive.Test
-- import qualified Diff.Guess.Script.Test
import qualified Diff.Guess.Term.Test
import GHC.IO.Encoding
-- import qualified Diff.Guess.Vernacular.Test
-- import qualified Diff.Motive.Test
-- import qualified Inductive.Eliminator.Test
import qualified Inductive.Inductive.Test
import Language
import Notations
import Parsing
import qualified Parsing.Inductive.Test
import Precedence
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.Term ()
import Term.Raw as Raw
import Term.Term
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Text.Megaparsec
import Text.Printf

-- import qualified StandardLibraryDiff.Test
-- import qualified Term.Term.Test

-- import qualified TestAlphaRenaming as TAR

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testGroup "(checked by HUnit)" $
        []
          ++ [unitTests]
          -- ++ [TAE.unitTests]
          -- ++ [TAR.unitTests]
          -- ++ [TF.unitTests]
          ++ [Diff.Guess.Term.Test.unitTests]
          ++ [Inductive.Inductive.Test.unitTests]
          ++ [Parsing.Inductive.Test.unitTests],
      localOption (SmallCheckDepth 3) $
        testGroup "(checked by SmallCheck)" [],
      -- ++ [TAE.scTests]
      -- ++ [TAR.scTests]
      -- ++ [TF.scTests]

      localOption (QuickCheckMaxSize 30) $
        localOption (QuickCheckTests 1000) $
          localOption (QuickCheckReplay Nothing) $
            localOption (QuickCheckShowReplay True) $
              --localOption (QuickCheckVerbose True) $
              testGroup "(checked by QuickCheck)" []
              -- ++ [TAE.qcTests]
              -- ++ [TAR.qcTests]
              -- ++ [TF.qcTests]
    ]

parseMaybeRaw :: String -> Maybe (Raw.Term Variable)
parseMaybeRaw = parseMaybe termP

prettyRaw :: Raw.Term Variable -> String
prettyRaw = prettyStr @'Chick

unitTestTerms :: [(Raw.Term Variable, String)]
unitTestTerms =
  [ (var "a" ^:: var "b", printf "a %s b" annotSymbol),
    (var "a" ^$ var "b", "a b"),
    (hole, holeSymbol),
    ( (^\) ["arg1", "arg2", "arg3"] (var "body"),
      printf "%s arg1 arg2 arg3%s body" lamSymbol postLamSymbol
    ),
    ( let' [("x1", var "body1"), ("x2", var "body2")] (var "body"),
      printf
        "let x1 %s body1 in let x2 %s body2 in body"
        postLetSymbol
        postLetSymbol
    ),
    ( π [("t", var "τ1")] (var "τ2"),
      printf "%s (t : τ1)%s τ2" forallSymbol postForallSymbol
    ),
    (var "τ1" ^-> var "τ2", "τ1 → τ2"),
    (type', "Type"),
    (var "foo", "foo")
  ]

unitTests :: TestTree
unitTests =
  let isTolerable' = isTolerable (tableToOrdering def)
   in let isNotTolerable' c p = not (isTolerable' c p)
       in testGroup "Unit tests" $
            [testCase ("parsing " ++ s) $ parseMaybeRaw s @?= Just t | (t, s) <- unitTestTerms]
              ++ [testCase ("printing " ++ s) $ prettyRaw t @?= s | (t, s) <- unitTestTerms]
              ++ [ let name = "isNotTolerable' PrecAnnot (PrecApp, TolerateHigher)"
                    in testCase name $
                         isNotTolerable' PrecAnnot (PrecApp, TolerateHigher) @? ":(",
                   let name = "isNotTolerable' PrecAnnot (PrecApp, TolerateEqual)"
                    in testCase name $
                         isNotTolerable' PrecAnnot (PrecApp, TolerateEqual) @? ":("
                 ]

-- Maybe it's fine to have "a : b : c"?
-- ++

-- [ let bad = printf "a %s b %s c" annotSymbol annotSymbol in
--   testCase (printf "should not parse %s" bad) $
--   parseMaybeRaw bad @?= Nothing ]

scProps :: TestTree
scProps =
  let isTolerable' = isTolerable (tableToOrdering def)
   in localOption (SmallCheckDepth 1) $
        testGroup
          "(checked by SmallCheck)"
          [ SC.testProperty
              "isTolerable' p (q, TolerateAny)"
              $ \p q -> isTolerable' p (q, TolerateAny),
            SC.testProperty
              "isTolerable' p (p, TolerateEqual)"
              $ \p -> isTolerable' p (p, TolerateEqual),
            SC.testProperty
              "not isTolerable' p (p, TolerateHigher)"
              $ \p -> not $ isTolerable' p (p, TolerateHigher)
              -- , SC.testProperty
              --   "parseMaybeRaw . prettyRaw == Just" $
              --   \ t -> wellFormed t SC.==> (parseMaybeRaw . prettyRaw) t == Just t
          ]

qcProps :: TestTree
qcProps =
  testGroup "Something something roundtrip" []

-- ++ [ QC.testProperty
--   "parseMaybeRaw . prettyRaw == Just" $
--   \ t -> wellFormed t QC.==> (parseMaybeRaw . prettyRaw) t == Just t
--    ]
