{-# options_ghc -fno-warn-unused-imports #-}

module Playground where

import Control.Monad
import Control.Monad.Except
import Data.Default
import Test.QuickCheck
import Text.PrettyPrint.Annotated.WL
import Text.Printf

import Parsing
import PrettyPrinting.Binder
import PrettyPrinting.Term
import PrettyPrinting.Utils
import StandardLibrary
import Tactic
import Term.AlphaRenaming
import Term.Fresh
import Term.Raw                      as Raw
import Term.Term
import Term.TypeChecked              as TypeChecked
import Term.TypeErrored              as TypeErrored
import Work

genPi :: Gen Raw.Term
genPi = do
  Pi def <$> arbitrary <*> arbitrary <*> arbitrary

test :: IO ()
test = do
  let task = checkF [] tId τId id
  --let task = checkF [] tFlip τFlip id
  let trace = tcTrace stepTypeCheckerF task
  forM_ trace $ \ item -> do
    putStrLn $ doc2String $ prettyTypeCheckerF item

typeCheck :: TermX ξ -> Maybe (TypeX ξ) -> IO ()
typeCheck t mτ = do
  putStrLn $ replicate 80 '-'
  putStrLn "Type-checking:"
  putStrLn $ prettyTerm t
  case mτ of
    Nothing -> return ()
    Just τ  -> do
      putStrLn "Against type:"
      putStrLn $ prettyTerm τ
  let e = tc $ case mτ of
        Nothing -> synthF [] t id
        Just τ  -> checkF [] t τ id
  case e of
    Left  l -> do
      putStrLn "Failed:"
      putStrLn $ prettyTerm l
      putStrLn $ getTypeError l
    Right r -> do
      putStrLn "Succeded:"
      putStrLn $ prettyTerm r
  return ()

randomTypeCheck :: IO ()
randomTypeCheck = do
  t <- generate (arbitrary :: Gen Raw.Term)
  typeCheck t Nothing

main :: IO ()
main = do
  l <- sample' (resize 2 (arbitrary :: Gen TypeChecked.Term))
  forM_ l $ \ p -> do
    putStrLn "\n\n\n"
    let g = Goal [] p
    printGoal g
    b <- generate arbitrary
    putStrLn $ printf "\nintros %s:\n" (prettyBinder b)
    eeg <- runExceptT $ runAtomic [] (Intro b) g
    case eeg of
      Left e   -> putStrLn e
      Right g' -> forM_ g' printGoal
  where
    printGoal = putStrLn . doc2String . prettyGoal ignoreAnnotations def
