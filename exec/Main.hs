{-# language LambdaCase #-}

module Main where

import Data.Monoid

import PrettyPrinting.PrettyPrintableUnannotated
import Typing.Free
import StandardLibrary (tId, τId)

main :: IO ()
main = do
  traceCheck tId τId >>= \case
    Left e        -> putStrLn $ "FAILURE: " <> show e
    Right (t, _γ) -> putStrLn $ "SUCCESS: " <> prettyStrU t
