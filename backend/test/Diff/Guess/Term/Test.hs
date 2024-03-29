{-# LANGUAGE TypeApplications #-}

module Diff.Guess.Term.Test
  ( unitTests,
  )
where

import Diff.Guess.Node
import Diff.Guess.Term
import qualified Diff.Term as ΔT
import Language
import PrettyPrinting.PrettyPrintable
import Repair.Benchmark
import qualified Term.Raw as Raw
import Term.Term
import Test.Tasty
import Test.Tasty.HUnit

_traceGuessδBench :: RepairTermBenchmark -> IO (ΔT.Diff Raw.Raw)
_traceGuessδBench b = traceGuessδ (repairTermFromType b) (repairTermToType b)

guessδBench :: RepairTermBenchmark -> ΔT.Diff Raw.Raw
guessδBench b = guessδ (repairTermFromType b) (repairTermToType b)

testBench :: RepairTermBenchmark -> Assertion
testBench b =
  let g = guessδBench b
   in ΔT.patchMaybe (repairTermFromType b) g @?= Just (repairTermToType b)

testTerms :: String -> String -> IO (ΔT.Diff Raw.Raw)
testTerms t1 t2 = do
  g <- traceGuessδ (unsafeParseRaw t1) (unsafeParseRaw t2)
  putStrLn $ show g
  return g

_testFlippedArguments :: IO ()
_testFlippedArguments = do
  let s1 = "f b y c d e"
  let s2 = "f a d b x c"
  let t1 = unsafeParseRaw s1
  δ <- testTerms s1 s2
  case ΔT.patchMaybe t1 δ of
    Nothing -> putStrLn "Patching failed"
    Just t2 -> putStrLn $ prettyStr @'Chick t2

_term1 :: IO (ΔT.Diff Raw.Raw)
_term1 =
  testTerms
    "∀ (h : A) (t : list A), list A"
    "∀ (h : A) (t : Vec A), Vec A"

unitTests :: TestTree
unitTests =
  testGroup "Diff.Guess.Term" $
    []
      ++ [testCase "bench1" $ testBench termBench1]
      ++ [testCase "bench2" $ testBench termBench2]
      ++ [testCase "bench3" $ testBench termBench3]
      ++ [testCase "bench4" $ testBench termBench4]
      ++ [testCase "bench5" $ testBench termBench5]

_test :: IO ()
_test = defaultMain unitTests

_testAnonymous :: IO ()
_testAnonymous = do
  let term = unsafeParseRaw "λ _ l, match l with | nil _ => O | cons _ _ _ => S O end"
  putStrLn $ show (term == term)
  res <- traceGuessδ term term
  putStrLn $ show res
  return ()

testMatchPairs :: Raw.Term Variable -> Raw.Term Variable -> IO [Match]
testMatchPairs = withNodeMapping $ \n1 n2 m -> do
  putStrLn $ show m
  let c1 = children n1
  let c2 = children n2
  putStrLn $ show c1
  putStrLn $ show c2
  return $ matchPairs m c1 c2

-- FIXME: right now, this generates Permuted [2, 1, 0] instead of Permuted [1, 2, 0]
-- which is correct but not what I'd prefer.
_testMatchPairs1 :: IO [Match]
_testMatchPairs1 = testMatchPairs (unsafeParseRaw "a b c d e") (unsafeParseRaw "x d b y")

-- FIXME: this should be a match but isn't for some reason
_testMatchPairs2 :: IO [Match]
_testMatchPairs2 = testMatchPairs (unsafeParseRaw "f") (unsafeParseRaw "f x")
