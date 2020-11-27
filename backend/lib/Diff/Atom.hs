{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff.Atom
  ( Diff(..)
  , patch
  , patchMaybe
  ) where

import Data.Aeson
import Prettyprinter
import GHC.Generics
import Polysemy                       ( Sem, run )

import PrettyPrinting.PrettyPrintable

data Diff a
  = Same
  | Replace a
  deriving (Eq, Functor, Generic, Show)

instance PrettyPrintable l a => PrettyPrintable l (Diff a) where
  prettyDoc Same        = "Same"
  prettyDoc (Replace r) = fillSep [ "Replace", prettyDoc @l r ]

instance ToJSON a => ToJSON (Diff a) where

patch ::
  a ->
  Diff a ->
  Sem r a
patch a d = case d of
  Same       -> return a
  Replace a' -> return a'

patchMaybe ::
  a ->
  Diff a ->
  Maybe a
patchMaybe a d = Just . run $ patch a d
