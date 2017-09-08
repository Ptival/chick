module TestDiff.Motive where

import qualified Diff.Inductive as DI
import           Diff.Motive
import qualified Diff.Term as DT
import           Inductive.Inductive
import           PrettyPrinting.PrettyPrintable
import           StandardLibrary
import           StandardLibraryDiff

test :: IO ()
test =
  let (Inductive  _n  _ps  is  _cs) = indList in
  let (DI.Modify _δn _δps δis _δcs) = δListToVec in
  let δis' = DI.instantiateΔis δis in
  let δ = δonInductiveIndexInside () δis' DT.Same in
  do
    putStrLn $ prettyStr δ
