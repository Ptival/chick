module Playground where

import Control.Monad
import Control.Monad.Except
import Data.Default
import Test.QuickCheck
import Text.PrettyPrint.Annotated.WL
import Text.Printf

import Parsing
import PrettyPrinting
import Tactic
import Term.AlphaRenaming
import Term.Fresh
import Term.RawTerm
import Term.Term

showDoc :: Doc a -> String
showDoc = display . renderPrettyDefault

genPi :: Gen RawTerm
genPi = do
  Pi def <$> arbitrary <*> arbitrary <*> arbitrary

main :: IO ()
main = do
  l1 <- sample' (resize 2 arbitrary :: Gen RawTerm)
  forM_ l1 $ \ t -> do
    putStrLn "---------------------"
    putStrLn $ showDoc $ prettyTerm def t
    let t' = αrename (Variable "a") (Variable "e") t
    putStrLn $ showDoc $ prettyTerm def t'

{-
  l <- sample' (resize 2 genPi)
  forM_ l $ \ p -> do
    putStrLn "\n\n\n"
    let g = Goal [] p
    printGoal g
    b <- generate arbitrary
    putStrLn $ printf "\nintros %s:\n" (showDoc $ prettyBinder b)
    eeg <- runExceptT $ runAtomic (Intro b) g
    case eeg of
      Left e   -> putStrLn e
      Right g' -> printGoal g'
  where printGoal = putStrLn . showDoc . prettyGoal def
-}
