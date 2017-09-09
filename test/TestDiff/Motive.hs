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
-- import qualified Term.Raw as Raw

test :: IO ()
test =
  let listMotive = mkMotiveType () indList Type in
  let  vecMotive = mkMotiveType () indVec  Type in
  let δlistMotive = δmkMotiveType () indList δListToVec in
  do
    putStrLn $ prettyStr listMotive
    putStrLn $ prettyStr δlistMotive
    putStrLn $ prettyStr vecMotive
    (runTrace . runError $ DT.patch listMotive δlistMotive) >>= \case
      Left s -> do
        putStrLn s
      Right motive' -> do
        putStrLn $ prettyStr motive'
