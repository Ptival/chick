module Inductive.Eliminator.Test where

import Control.Monad
import Inductive.Eliminator
import Inductive.Inductive
import Language
import PrettyPrinting.PrettyPrintable
import StandardLibrary
import Term.Term
import Text.Printf

displayEliminator :: Inductive () Variable -> IO ()
displayEliminator ind@(Inductive n _ _ _ _) =
  let elimName = mkEliminatorName n
   in let elimType = mkEliminatorType () ind
       in putStrLn $ printf "\n%s : %s" (prettyStr @'Chick elimName) (prettyStr @'Chick elimType)

main :: IO ()
main = do
  forM_ inductives $ \ind -> do
    displayEliminator ind
