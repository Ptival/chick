module Diff.Guess.Term.Test where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Diff.Term as ΔT
import           Diff.Guess.Term
import qualified Term.Raw as Raw
import           Repair.Benchmark

traceGuessδBench :: RepairTermBenchmark -> IO (ΔT.Diff Raw.Raw)
traceGuessδBench b = traceGuessδ (repairTermFromType b) (repairTermToType b)

guessδBench :: RepairTermBenchmark -> ΔT.Diff Raw.Raw
guessδBench b = guessδ (repairTermFromType b) (repairTermToType b)

testBench :: RepairTermBenchmark -> Assertion
testBench b =
  let g = guessδBench b in
  ΔT.patchMaybe (repairTermFromType b) g @?= Just (repairTermToType b)

unitTests :: TestTree
unitTests = testGroup "Diff.Guess.Term" $ []
  ++ [testCase "bench1" $ testBench termBench1 ]
  ++ [testCase "bench2" $ testBench termBench2 ]
  ++ [testCase "bench3" $ testBench termBench3 ]
  ++ [testCase "bench4" $ testBench termBench4 ]
  ++ [testCase "bench5" $ testBench termBench5 ]

test :: IO ()
test = defaultMain unitTests

testAnonymous :: IO ()
testAnonymous = do
  let term = unsafeParseRaw "λ _ l, match l with | nil _ => O | cons _ _ _ => S O end"
  putStrLn $ show (term == term)
  res <- traceGuessδ term term
  putStrLn $ show res
  return ()
