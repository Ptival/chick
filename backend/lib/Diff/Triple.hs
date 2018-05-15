{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Diff.Triple
  ( Diff(..)
  , extract1
  , extract2
  , extract3
  , patch
  , patchMaybe
  ) where

import Data.Aeson
import GHC.Generics
import Data.Text.Prettyprint.Doc

import PrettyPrinting.PrettyPrintable

data Diff δ1 δ2 δ3
  = Same
  | Modify δ1 δ2 δ3
  deriving (Eq, Generic, Show)

instance (ToJSON δ1, ToJSON δ2, ToJSON δ3) => ToJSON (Diff δ1 δ2 δ3) where

extract1 :: δ1 -> Diff δ1 δ2 δ3 -> δ1
extract1 same Same           = same
extract1 _    (Modify δ _ _) = δ

extract2 :: δ2 -> Diff δ1 δ2 δ3 -> δ2
extract2 same Same           = same
extract2 _    (Modify _ δ _) = δ

extract3 :: δ3 -> Diff δ1 δ2 δ3 -> δ3
extract3 same Same           = same
extract3 _    (Modify _ _ δ) = δ

instance
  (PrettyPrintable l δ1, PrettyPrintable l δ2, PrettyPrintable l δ3) =>
  PrettyPrintable l (Diff δ1 δ2 δ3) where
  prettyDoc Same              = "Same"
  prettyDoc (Modify δ1 δ2 δ3) =
    encloseSep lparen rparen comma
    [ prettyDoc @l δ1
    , prettyDoc @l δ2
    , prettyDoc @l δ3
    ]

patch ::
  Monad m =>
  (a -> δa -> m a) ->
  (b -> δb -> m b) ->
  (c -> δc -> m c) ->
  (a, b, c) -> Diff δa δb δc -> m (a, b, c)
patch pA pB pC (a, b, c) = \case
  Same            -> return (a, b, c)
  Modify δa δb δc -> (,,) <$> pA a δa <*> pB b δb <*> pC c δc

patchMaybe ::
  (a -> δa -> Maybe a) ->
  (b -> δb -> Maybe b) ->
  (c -> δc -> Maybe c) ->
  (a, b, c) -> Diff δa δb δc -> Maybe (a, b, c)
patchMaybe = patch
