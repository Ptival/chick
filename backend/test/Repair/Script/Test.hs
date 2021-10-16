module Repair.Script.Test
  (
  )
where

import Diff.Guess.Script.Test (δScriptSF)
import Examples.Diff.SoftwareFoundations (scriptBefore)
import Language (Language (Chick))
import Polysemy (runM)
import Polysemy.Trace (traceToIO)
import PrettyPrinting.PrettyPrintableUnannotated
import Repair.Script (runRepair')
import Script
import qualified Term.Raw as Raw
import Term.Term

-- TODO: move patchProof from Repair.Benchmark to Repair.Script? Seems unrelated
-- to benchmarks
foo :: IO (Either String (Script Raw.Raw Variable))
foo = runM . traceToIO $ runRepair' scriptBefore δScriptSF

bar :: IO ()
bar = do
  foo >>= \case
    Right s -> do
      putStrLn $ prettyStrU @'Chick s
    Left e -> do
      putStrLn e
