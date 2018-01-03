{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Diff.Guess.Inductive.Test where

import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec
import           Text.Printf

import           Diff.Guess.Inductive
import qualified Diff.Inductive as ΔI
import           Inductive.Inductive
import           Parsing.Inductive
import           PrettyPrinting.PrettyPrintable
import           StandardLibrary
import qualified Term.Raw as Raw
import           Term.Variable

unsafeParseInductive :: [String] -> Inductive Raw.Raw Variable
unsafeParseInductive ss =
  let s = unlines ss in
  case parseMaybe inductiveP s of
    Nothing -> error $ printf "unsafeParseInductive: could not parse\n%s" s
    Just t  -> t

indList1 :: Inductive Raw.Raw Variable
indList1 = unsafeParseInductive
  [ "Inductive Vec (A : Type) : Type :="
  , "| nil : Vec A"
  , "| cons : ∀ (h : A) (t : Vec A), Vec A"
  ]

sanityCheck :: Inductive Raw.Raw Variable -> Inductive Raw.Raw Variable -> Assertion
sanityCheck i1 i2 = do
  δ <- runTrace $ guess i1 i2
  runTrace (runError (ΔI.patch i1 δ)) >>= \case
    Left (_ :: String) -> assertFailure "sanity check failed"
    Right i2'          -> i2' @?= i2

mkSanityCheck :: Inductive Raw.Raw Variable -> Inductive Raw.Raw Variable -> TestTree
mkSanityCheck i1 i2 =
  testCase
  (printf "%s -> %s" (prettyStr $ inductiveName i1) (prettyStr $ inductiveName i2))
  (sanityCheck i1 i2)

sanityChecks :: [TestTree]
sanityChecks = [ mkSanityCheck i1 i2 | i1 <- inductives, i2 <- inductives ]

unitTests :: TestTree
unitTests = testGroup "Diff.Guess.Inductive" $ []
  ++ sanityChecks

test :: IO ()
test = defaultMain unitTests

{-
Inductive {inductiveName = Variable {unVariable = "eq"}, inductiveParameters = [((),Variable {unVariable = "A"},Type Type),((),Variable {unVariable = "x"},Var Nothing (Variable {unVariable = "A"}))], inductiveIndices = [((),Variable {unVariable = "_"},Var Nothing (Variable {unVariable = "A"}))], inductiveUniverse = Prop, inductiveConstructors = [Constructor _ Variable {unVariable = "eq_refl"} [] [((),Var Nothing (Variable {unVariable = "x"}))]]}
Inductive {inductiveName = Variable {unVariable = "eq"}, inductiveParameters = [((),Variable {unVariable = "_"},Type Type),((),Variable {unVariable = "_"},Var Nothing (Variable {unVariable = "A"}))], inductiveIndices = [((),Variable {unVariable = "_"},Var Nothing (Variable {unVariable = "A"}))], inductiveUniverse = Prop, inductiveConstructors = [Constructor _ Variable {unVariable = "eq_refl"} [] [],Constructor _ Variable {unVariable = "false"} [] []]}
-}
