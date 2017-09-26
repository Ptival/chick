{-# language DeriveFoldable #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}

module Diff.Triple
  ( Diff(..)
  , patch
  , patchMaybe
  ) where

import Text.PrettyPrint.Annotated.WL

import PrettyPrinting.PrettyPrintable

data Diff δ1 δ2 δ3
  = Same
  | Modify δ1 δ2 δ3
  deriving (Show)

instance (PrettyPrintable δ1, PrettyPrintable δ2, PrettyPrintable δ3) =>
         PrettyPrintable (Diff δ1 δ2 δ3) where
  prettyDoc Same              = text "Same"
  prettyDoc (Modify δ1 δ2 δ3) =
    encloseSep lparen rparen comma
    [ prettyDoc δ1
    , prettyDoc δ2
    , prettyDoc δ3
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
