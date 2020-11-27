{-# LANGUAGE AllowAmbiguousTypes #-}

module Diff.Guess.Atom
  ( guess,
  )
where

import Control.Monad ()
import Data.Function ()
import qualified Diff.Atom as ΔA
import Polysemy (Sem)
import PrettyPrinting.Term ()
import Prelude hiding (product)

guess ::
  Eq a =>
  a ->
  a ->
  Sem r (ΔA.Diff a)
guess v1 v2 =
  return $
    if v1 == v2
      then ΔA.Same
      else ΔA.Replace v2
