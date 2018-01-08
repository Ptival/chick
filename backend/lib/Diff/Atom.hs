{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Diff.Atom
  ( Diff(..)
  , patch
  , patchMaybe
  ) where

import Control.Monad.Freer
import Data.Aeson
import GHC.Generics
import Text.PrettyPrint.Annotated.WL

import PrettyPrinting.PrettyPrintable

data Diff a
  = Same
  | Replace a
  deriving (Eq, Functor, Generic, Show)

instance PrettyPrintable a => PrettyPrintable (Diff a) where
  prettyDoc Same        = text "Same"
  prettyDoc (Replace r) = fillSep [ text "Replace", prettyDoc r ]

instance ToJSON a => ToJSON (Diff a) where

patch :: a -> Diff a -> Eff r a
patch a d = case d of
  Same       -> return a
  Replace a' -> return a'

patchMaybe :: a -> Diff a -> Maybe a
patchMaybe a d = Just . run $ patch a d