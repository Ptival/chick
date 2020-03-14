{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module Diff.Guess.Atom
  ( guess
  ) where

import           Control.Monad (  )
import           Polysemy ( Sem )
import           Data.Function (  )
import           Prelude hiding ( product )

import qualified Diff.Atom as ΔA
import           PrettyPrinting.Term ()

guess ::
  Eq a =>
  a -> a -> Sem r (ΔA.Diff a)
guess v1 v2 =
  return $ if v1 == v2
           then ΔA.Same
           else ΔA.Replace v2
