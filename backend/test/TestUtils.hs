module TestUtils
  ( equalityCheck
  , inequalityCheck
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf

import PrettyPrinting.PrettyPrintable

equalityCheck :: (Eq a, PrettyPrintable a) => a -> TestTree
equalityCheck t =
  testCase
  (printf "%s == %s" (preview t) (preview t))
  $ (t == t) @? "a value did not equate itself"

inequalityCheck :: (Eq a, PrettyPrintable a) => a -> a -> TestTree
inequalityCheck t1 t2 =
  testCase
  (printf "%s /= %s" (preview t1) (preview t2))
  $ (t1 /= t2) @? "two distinct values equated each other"
