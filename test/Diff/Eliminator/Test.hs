{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Diff.Eliminator.Test where

import           Control.Monad.Freer.Exception

import           Diff.Eliminator
import           Diff.Motive
import qualified Diff.Term as DT
import           Inductive.Eliminator
import           Inductive.Motive
import           PrettyPrinting.PrettyPrintable
import           StandardLibrary
import           StandardLibraryDiff
import           Term.Term
import           Utils

testListToVec :: IO Bool
testListToVec =
  let listEliminator = mkEliminatorType () indList in
  let  vecEliminator = mkEliminatorType () indVec  in
  case δmkEliminatorType () indList δListToVec of
  Nothing -> do
    putStrLn "δmkEliminatorType failed"
    return False
  Just δlistEliminator -> do
    putStrLn $ "Eliminator before:"
    putStrLn $ prettyStr listEliminator
    putStrLn $ replicate 80 '-'
    putStrLn $ "δ:"
    putStrLn $ prettyStr δlistEliminator
    putStrLn $ replicate 80 '-'
    putStrLn $ "Eliminator expected:"
    putStrLn $ prettyStr vecEliminator
    putStrLn $ replicate 80 '-'
    putStrLn $ "Eliminator obtained:"

    (runSkipTrace . runError $ DT.patch listEliminator δlistEliminator) >>= \case
      Left (s :: String) -> do
        putStrLn "An error occurred:"
        putStrLn s
        return False
      Right vecEliminator' -> do
        putStrLn $ prettyStr vecEliminator'
        return (vecEliminator' == vecEliminator)
