{-# LANGUAGE DataKinds #-}

module Diff.Atom
  ( Diff(..)
  , patch
  , patchMaybe
  ) where

import Control.Monad.Freer
import Text.PrettyPrint.Annotated.WL

import PrettyPrinting.PrettyPrintable

data Diff a
  = Same
  | Replace a
  deriving (Show)

instance PrettyPrintable a => PrettyPrintable (Diff a) where
  prettyDoc Same        = text "Same"
  prettyDoc (Replace r) = fillSep [ text "Replace", prettyDoc r ]

patch :: a -> Diff a -> Eff r a
patch a d = case d of
  Same       -> return a
  Replace a' -> return a'

patchMaybe :: a -> Diff a -> Maybe a
patchMaybe a d = Just . run $ patch a d
