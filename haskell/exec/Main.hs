{-# language LambdaCase #-}

module Main where

import PrettyPrinting.PrettyPrintableUnannotated
import Typing.Free
import StandardLibrary

main :: IO ()
main = do
  foo >>= \case
    Left e       -> putStrLn $ show e
    Right (t, γ) -> putStrLn $ prettyStrU t
