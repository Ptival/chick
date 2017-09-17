module Inductive.Eliminator.Test where

import Control.Monad
import Text.Printf

import Inductive.Eliminator
import Inductive.Inductive
import PrettyPrinting.PrettyPrintable
import StandardLibrary
import Term.Term

displayEliminator :: Inductive () Variable -> IO ()
displayEliminator ind@(Inductive n _ _ _) =
  let elimName = eliminatorName n in
  let elimType = mkEliminatorType () ind in
  putStrLn $ printf "\n%s : %s" (prettyStr elimName) (prettyStr elimType)

main :: IO ()
main = do
  forM_ inductives $ \ ind -> do
    displayEliminator ind
