module Diff.Guess.Constructor.Test where

import           Control.Monad.Freer
import           Control.Monad.Freer.Trace
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
  , "| nil : Vec A"
  , "| cons : ∀ (h : A) (t : Vec A), Vec A"
  ]

guessNil =
  let l0 = inductiveConstructors indList in
  let l1 = inductiveConstructors indList1 in
  runTrace $ ΔGC.guess (l0 !! 0) (l1 !! 0)

guessCons =
  let l0 = inductiveConstructors indList in
  let l1 = inductiveConstructors indList1 in
  do
    putStrLn $ show $ l0 == l1
    runTrace $ ΔGC.guess (l0 !! 1) (l1 !! 1)

-- testBench :: RepairTermBenchmark -> Assertion
-- testBench b =
--   let g = guessδBench b in
--   ΔT.patchMaybe (repairTermFromType b) g @?= Just (repairTermToType b)

-- unitTests :: TestTree
-- unitTests = testGroup "Diff.Guess.Constructor" $ []
--   ++ [testCase "bench1" $ testBench termBench1 ]
--   ++ [testCase "bench2" $ testBench termBench2 ]
--   ++ [testCase "bench3" $ testBench termBench3 ]
--   ++ [testCase "bench4" $ testBench termBench4 ]
--   ++ [testCase "bench5" $ testBench termBench5 ]

-- test :: IO ()
-- test = defaultMain unitTests
