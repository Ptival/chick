{-# options_ghc -fno-warn-unused-imports #-}

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
import Work

genPi :: Gen RawTerm
genPi = do
  Pi def <$> arbitrary <*> arbitrary <*> arbitrary

randomTypeCheck :: IO ()
randomTypeCheck = do
  t <- generate (arbitrary :: Gen RawTerm)
  putStrLn $ prettyTerm t
  let e = tc $ synthF [] t id
  case e of
    Left  l -> do
      putStrLn "Type-checking failed:"
      putStrLn $ prettyTerm l
    Right r -> do
      putStrLn "Type-checking succeded:"
      putStrLn $ prettyTerm r
  return ()

{-
main :: IO ()
main = do
  l <- sample' (resize 2 (arbitrary :: Gen RawTerm))
  forM_ l $ \ p -> do
    putStrLn "\n\n\n"
    let g = Goal [] p
    printGoal g
    b <- generate arbitrary
    putStrLn $ printf "\nintros %s:\n" (doc2String $ prettyBinder b)
    eeg <- runExceptT $ runAtomic (Intro b) g
    case eeg of
      Left e   -> putStrLn e
      Right g' -> printGoal g'
  where printGoal = putStrLn . doc2String . prettyGoal def
-}
