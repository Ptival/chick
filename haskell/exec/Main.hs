module Main where

import PrettyPrinting.PrettyPrintableUnannotated
import StandardLibrary
-- import Work

main :: IO ()
main = do
  putStrLn $ prettyStrU stdlib
