module Diff.Guess.Constructor.Test where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec
import           Text.Printf

import qualified Diff.Term as ΔT
import qualified Diff.Guess.Constructor as ΔGC
import           Inductive.Inductive
import           Parsing.Inductive
import qualified Term.Raw as Raw
import           Repair.Benchmark
import           StandardLibrary
import           Term.Variable

unsafeParseInductive :: [String] -> Inductive Raw.Raw Variable
unsafeParseInductive ss =
  let s = unlines ss in
  case parseMaybe inductiveP s of
    Nothing -> error $ printf "unsafeParseInductive: could not parse\n%s" s
    Just t  -> t

indList1 :: Inductive Raw.Raw Variable
indList1 = unsafeParseInductive
  [ "Inductive Vec (A : Type) : Type :="
  , "| nil : list A"
  , "| cons : ∀ (x : A) (xs : list A), list A"
  ]

traceGuessδBench :: RepairTermBenchmark -> IO (ΔT.Diff Raw.Raw)
traceGuessδBench b = traceGuessδ (repairTermFromType b) (repairTermToType b)

guessδBench :: RepairTermBenchmark -> ΔT.Diff Raw.Raw
guessδBench b = guessδ (repairTermFromType b) (repairTermToType b)

testBench :: RepairTermBenchmark -> Assertion
testBench b =
  let g = guessδBench b in
  ΔT.patchMaybe (repairTermFromType b) g @?= Just (repairTermToType b)

unitTests :: TestTree
unitTests = testGroup "Diff.Guess.Constructor" $ []
  ++ [testCase "bench1" $ testBench termBench1 ]
  ++ [testCase "bench2" $ testBench termBench2 ]
  ++ [testCase "bench3" $ testBench termBench3 ]
  ++ [testCase "bench4" $ testBench termBench4 ]
  ++ [testCase "bench5" $ testBench termBench5 ]

test :: IO ()
test = defaultMain unitTests
