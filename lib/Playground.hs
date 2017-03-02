module Playground where

import Control.Monad
import Control.Monad.Except
import Data.Default
import Test.QuickCheck
import Text.PrettyPrint.Annotated.WL
import Text.Printf

import Tactic
import Term.RawTerm
import Term.Term

showDoc :: Doc a -> String
showDoc = display . renderPrettyDefault

genPi :: Gen RawTerm
genPi = do
  Pi def <$> arbitrary <*> arbitrary <*> arbitrary

main :: IO ()
main = do
  l <- sample' (resize 1 genPi)
  forM_ l $ \ p -> do
    putStrLn "\n\n\n"
    let g = Goal [] p
    printGoal g
    b <- generate arbitrary
    putStrLn $ printf "\nintros %s:\n" (show b)
    eeg <- runExceptT $ runAtomic (Intro b) g
    case eeg of
      Left e   -> putStrLn e
      Right g' -> printGoal g'
  where printGoal = putStrLn . showDoc . prettyGoal def
