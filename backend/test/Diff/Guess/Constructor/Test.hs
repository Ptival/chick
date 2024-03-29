module Diff.Guess.Constructor.Test where

import qualified Diff.Constructor as ΔC
import qualified Diff.Guess.Constructor as ΔGC
import Inductive.Inductive
import Parsing.Inductive
import Polysemy
import Polysemy.Trace
import StandardLibrary
import qualified Term.Raw as Raw
import Term.Variable
import Text.Megaparsec
import Text.Printf

unsafeParseInductive :: [String] -> Inductive Raw.Raw Variable
unsafeParseInductive ss =
  let s = unlines ss
   in case parseMaybe inductiveP s of
        Nothing -> error $ printf "unsafeParseInductive: could not parse\n%s" s
        Just t -> t

indList1 :: Inductive Raw.Raw Variable
indList1 =
  unsafeParseInductive
    [ "Inductive Vec (A : Type) : Type :=",
      "| nil : Vec A",
      "| cons : ∀ (h : A) (t : Vec A), Vec A"
    ]

guessNil :: IO (ΔC.Diff Raw.Raw)
guessNil =
  let l0 = inductiveConstructors indList
   in let l1 = inductiveConstructors indList1
       in runM . traceToStdout $ ΔGC.guess (head l0) (head l1)

guessCons :: IO (ΔC.Diff Raw.Raw)
guessCons =
  let l0 = inductiveConstructors indList
   in let l1 = inductiveConstructors indList1
       in do
            print (l0 == l1)
            runM . traceToStdout $ ΔGC.guess (l0 !! 1) (l1 !! 1)

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
