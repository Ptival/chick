{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff.Pair
  ( Diff (..),
    δfst,
    δsnd,
    patch,
    patchMaybe,
  )
where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyDoc),
  )
import Prettyprinter (comma, fillSep, lparen, rparen)

data Diff δl δr
  = Same
  | Modify δl δr
  deriving (Eq, Generic, Show)

instance (ToJSON δ1, ToJSON δ2) => ToJSON (Diff δ1 δ2)

instance (PrettyPrintable l a, PrettyPrintable l b) => PrettyPrintable l (Diff a b) where
  prettyDoc Same = "Same"
  prettyDoc (Modify a b) =
    fillSep [lparen, prettyDoc @l a, comma, prettyDoc @l b, rparen]

patch ::
  Monad m =>
  (l -> δl -> m l) ->
  (r -> δr -> m r) ->
  (l, r) ->
  Diff δl δr ->
  m (l, r)
patch patchL patchR (l, r) = \case
  Same -> return (l, r)
  Modify δl δr -> (,) <$> patchL l δl <*> patchR r δr

patchMaybe ::
  (l -> δl -> Maybe l) ->
  (r -> δr -> Maybe r) ->
  (l, r) ->
  Diff δl δr ->
  Maybe (l, r)
patchMaybe = patch

δfst :: δl -> Diff δl δr -> δl
δfst same Same = same
δfst _ (Modify δl _) = δl

δsnd :: δr -> Diff δl δr -> δr
δsnd same Same = same
δsnd _ (Modify _ δr) = δr
