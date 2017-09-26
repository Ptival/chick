{-# language DeriveFoldable #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}

module Diff.Pair
  ( Diff(..)
  , δfst
  , δsnd
  , patch
  , patchMaybe
  ) where

-- import Control.Monad.Freer
-- import Control.Monad.Freer.Exception
import Text.PrettyPrint.Annotated.WL

import PrettyPrinting.PrettyPrintable

data Diff δl δr
  = Same
  | Modify δl δr
  deriving (Show)

instance (PrettyPrintable l, PrettyPrintable r) => PrettyPrintable (Diff l r) where
  prettyDoc Same         = text "Same"
  prettyDoc (Modify l r) = fillSep [ lparen, prettyDoc l, comma, prettyDoc r, rparen ]

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
