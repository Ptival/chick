{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Diff.Pair
  ( Diff(..)
  , δfst
  , δsnd
  , patch
  , patchMaybe
  ) where

import Data.Text.Prettyprint.Doc

import PrettyPrinting.PrettyPrintable

data Diff δl δr
  = Same
  | Modify δl δr
  deriving (Eq, Show)

instance (PrettyPrintable l a, PrettyPrintable l b) => PrettyPrintable l (Diff a b) where
  prettyDoc Same         = "Same"
  prettyDoc (Modify a b) =
    fillSep [ lparen, prettyDoc @l a, comma, prettyDoc @l b, rparen ]

patch ::
  Monad m =>
  (l -> δl -> m l) ->
  (r -> δr -> m r) ->
  (l, r) ->
  Diff δl δr ->
  m (l, r)
patch patchL patchR (l, r) = \case
  Same         -> return (l, r)
  Modify δl δr -> (,) <$> patchL l δl <*> patchR r δr

patchMaybe ::
  (l -> δl -> Maybe l) ->
  (r -> δr -> Maybe r) ->
  (l, r) ->
  Diff δl δr ->
  Maybe (l, r)
patchMaybe = patch

δfst :: δl -> Diff δl δr -> δl
δfst same Same           = same
δfst _    (Modify δl _)  = δl

δsnd :: δr -> Diff δl δr -> δr
δsnd same Same          = same
δsnd _    (Modify _ δr) = δr
