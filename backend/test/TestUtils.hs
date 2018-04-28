{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module TestUtils
  ( equalityCheck
  , inequalityCheck
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf

import PrettyPrinting.PrettyPrintable

equalityCheck :: ∀ l a. (Eq a, PrettyPrintable l a) => a -> TestTree
equalityCheck t =
  testCase
  (printf "%s == %s" (preview @l t) (preview @l t))
  $ (t == t) @? "a value did not equate itself"

inequalityCheck :: ∀ l a. (Eq a, PrettyPrintable l a) => a -> a -> TestTree
inequalityCheck t1 t2 =
  testCase
  (printf "%s /= %s" (preview @l t1) (preview @l t2))
  $ (t1 /= t2) @? "two distinct values equated each other"
