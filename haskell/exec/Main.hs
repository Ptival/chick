{-# language LambdaCase #-}

module Main where

import Data.Monoid

import PrettyPrinting.PrettyPrintableUnannotated
import Typing.Free
--import StandardLibrary (foo)

main :: IO ()
main = do
  foo >>= \case
    Left e       -> putStrLn $ "FAILURE: " <> show e
    Right (t, γ) -> putStrLn $ "SUCCESS: " <> prettyStrU t
