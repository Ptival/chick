{-# LANGUAGE LambdaCase #-}

module TestDiff.Motive where

import Control.Monad.Freer.Exception
import Control.Monad.Freer.Trace

import qualified Diff.Inductive as DI
import           Diff.Motive
import qualified Diff.Term as DT
import           Inductive.Inductive
import           Inductive.Motive
import           PrettyPrinting.PrettyPrintable
import           StandardLibrary
import           StandardLibraryDiff
import           Term.Term
import qualified Term.Raw as Raw

test :: IO ()
test =
  let (Inductive  n  ps  is  _cs) = indList in
  let (DI.Modify _δn δps δis _δcs) = δListToVec in
  -- let δis' = DI.instantiateΔis δis in
  let  motive  =  mkMotiveType' () n  ps  is Type in
  let δmotive  = δmkMotiveType' ()   δps δis in
  let vecMotive = mkMotiveType () indVec Type in
  do
    putStrLn $ prettyStr motive
    putStrLn $ prettyStr δmotive
    putStrLn $ prettyStr vecMotive
    (runTrace . runError $ DT.patch motive δmotive) >>= \case
      Left s -> do
        putStrLn s
      Right motive' -> do
        putStrLn $ prettyStr motive'
