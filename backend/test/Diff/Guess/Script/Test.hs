module Diff.Guess.Script.Test where

import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Text.Printf

import qualified Diff.Script as ΔS
import qualified Diff.Guess.Script as ΔGS
import qualified Examples.Diff.SoftwareFoundations as EDSF
import           Repair.Benchmark
import           Script
import qualified Term.Raw as Raw
import           Term.Term
import           Utils

testBenchmark :: RepairScriptBenchmark -> IO ()
testBenchmark b = do
  let s1 = repairScriptFromScript b
  let δs = repairScriptDiff b
  es2 <- runTrace . runError $ ΔS.patch s1 δs
  case es2 of
    Left e -> error $ printf "The benchmark's diff is broken: %s" (e :: String)
    Right s2 -> do
      result <- runTrace $ ΔGS.guess s1 s2
      putStrLn "SUCCESS:"
      putStrLn $ show result

testListVec :: IO ()
testListVec = testBenchmark repairListToVec

testFlippedArguments :: IO ()
testFlippedArguments = testBenchmark repairFlippedArguments

δScripts :: Script Raw.Raw Variable -> Script Raw.Raw Variable -> ΔS.Diff Raw.Raw
δScripts s1 s2 = skipTrace $ ΔGS.guess s1 s2

δScriptSF :: ΔS.Diff Raw.Raw
δScriptSF = δScripts EDSF.scriptBefore EDSF.scriptAfter
