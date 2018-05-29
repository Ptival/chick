{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Diff.Maybe
  ( Diff(..)
  , patch
  -- , patchMaybe
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Exception
import Data.Aeson
import GHC.Generics

import Diff.Utils
import PrettyPrinting.PrettyPrintable

data Diff e δe
  = Same
  | BecomeNothing
  | BecomeJust e
  | ModifyJust δe
  deriving (Eq, Generic, Show)

instance (ToJSON e, ToJSON δe) => ToJSON (Diff e δe) where

instance (PrettyPrintable l δe) => PrettyPrintable l (Diff e δe) where
  prettyDoc Same             = "Same"
  prettyDoc _                = "TODO: prettyprint Diff.Maybe"

patch ::
  ( Member (Exc String) r
  ) =>
  (e -> δe -> Eff r e) ->
  Maybe e ->
  Diff e δe ->
  Eff r (Maybe e)
patch patchE me = \case
  Same          -> return me
  BecomeNothing -> return Nothing
  BecomeJust e' -> return $ Just e'
  ModifyJust δe ->
    case me of
    Nothing -> throwExc $ "[Diff.Maybe.patch] ModifyJust on Nothing"
    Just e -> Just <$> patchE e δe

-- patchMaybe ::
--   (l -> δl -> Maybe l) ->
--   (r -> δr -> Maybe r) ->
--   (l, r) ->
--   Diff δl δr ->
--   Maybe (l, r)
-- patchMaybe = patch
