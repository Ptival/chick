module Diff.Guess.Term.Test where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Diff.Term as ΔT
import           Diff.Guess.Term
import qualified Term.Raw as Raw
import           Repair.Benchmark

traceGuessδBench :: RepairTermBenchmark -> IO (Maybe (ΔT.Diff Raw.Raw))
traceGuessδBench b = traceGuessδ (repairTermFromType b) (repairTermToType b)

guessδBench :: RepairTermBenchmark -> Maybe (ΔT.Diff Raw.Raw)
guessδBench b = guessδ (repairTermFromType b) (repairTermToType b)

testBench :: RepairTermBenchmark -> Assertion
testBench b = guessδBench b @?= Just (repairTermDiff b)

unitTests :: TestTree
unitTests = testGroup "Diff.Guess.Term" $ []
  ++ [testCase "bench1" $ testBench termBench1 ]
  ++ [testCase "bench2" $ testBench termBench2 ]
  ++ [testCase "bench3" $ testBench termBench3 ]
  ++ [testCase "bench4" $ testBench termBench4 ]
  ++ [testCase "bench5" $ testBench termBench5 ]

test :: IO ()
test = defaultMain unitTests
