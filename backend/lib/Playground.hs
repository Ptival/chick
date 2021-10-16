module Playground where

import Control.Monad
-- import Control.Monad.Except
import Control.Monad.Reader
import Data.Default
-- import Test.QuickCheck
-- import Text.PrettyPrint.Annotated.WL
-- import Text.Printf

import Language
-- import Parsing
-- import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import PrettyPrinting.Utils
import StandardLibrary
-- import Tactic
-- import Term.AlphaRenaming
-- import Term.Fresh
-- import Term.Raw                                  as Raw
import Term.Term
-- import Term.TypeChecked                          as TypeChecked
-- import Term.TypeErrored                          as TypeErrored
-- import Term.Variable
-- import Typing.GlobalEnvironment
import Typing.LocalContext
import Work

{-
genPi :: Gen (Raw.Term ν)
genPi = do
  Pi () <$> arbitrary <*> arbitrary
-}

test :: IO ()
test = do
  -- let task = checkF (LocalContext []) tId τId id
  let task = checkF (LocalContext []) tFlip τFlip id
  let trace = tcTrace stepTypeCheckerF task
  forM_ trace $ \item ->
    putStrLn $ doc2String $ runReader (prettyTypeCheckerF item) def

typeCheck :: TermX ξ Variable -> Maybe (TypeX ξ Variable) -> IO ()
typeCheck t mτ = do
  putStrLn $ replicate 80 '-'
  putStrLn "Type-checking:"
  putStrLn $ prettyStrU @'Chick t
  case mτ of
    Nothing -> return ()
    Just τ -> do
      putStrLn "Against type:"
      putStrLn $ prettyStrU @'Chick τ
  let e = tc $ case mτ of
        Nothing -> synthF (LocalContext []) t id
        Just τ -> checkF (LocalContext []) t τ id
  case e of
    Left l -> do
      putStrLn "Failed:"
      putStrLn $ prettyStrU @'Chick l
    Right r -> do
      putStrLn "Succeded:"
      putStrLn $ prettyStrU @'Chick r
  return ()

{-
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
-}
