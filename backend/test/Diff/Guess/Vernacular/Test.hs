module Diff.Guess.Vernacular.Test where

import Control.Monad.Freer.Exception
import Control.Monad.Freer.Trace
import Text.Printf

import qualified Diff.Vernacular as ΔV
import Diff.Guess.Script
import Repair.Benchmark

testBenchmark :: RepairScriptBenchmark -> IO ()
testBenchmark b = do
  let s1 = repairScriptFromScript b
  let δs = repairScriptDiff b
  es2 <- runTrace . runError $ ΔS.patch s1 δs
  case es2 of
    Left e -> error $ printf "The benchmark's diff is broken: %s" (e :: String)
    Right s2 -> do
      result <- runTrace $ guess s1 s2
      putStrLn "SUCCESS:"
      putStrLn $ show result

testListVec :: IO ()
testListVec = testBenchmark repairListToVec

testFlippedArguments :: IO ()
testFlippedArguments = testBenchmark repairFlippedArguments
