{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff.Maybe
  ( Diff (..),
    patch,
    -- , patchMaybe
  )
where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyDoc),
  )

data Diff e δe
  = Same
  | BecomeNothing
  | BecomeJust e
  | ModifyJust δe
  deriving (Eq, Generic, Show)

instance
  ( ToJSON e,
    ToJSON δe
  ) =>
  ToJSON (Diff e δe)

instance
  ( PrettyPrintable l δe
  ) =>
  PrettyPrintable l (Diff e δe)
  where
  prettyDoc Same = "Same"
  prettyDoc _ = "TODO: prettyprint Diff.Maybe"

patch ::
  Member (Error String) r =>
  (e -> δe -> Sem r e) ->
  Maybe e ->
  Diff e δe ->
  Sem r (Maybe e)
patch patchE me = \case
  Same -> return me
  BecomeNothing -> return Nothing
  BecomeJust e' -> return $ Just e'
  ModifyJust δe ->
    case me of
      Nothing -> throw ("[Diff.Maybe.patch] ModifyJust on Nothing" :: String)
      Just e -> Just <$> patchE e δe

-- patchMaybe ::
--   (l -> δl -> Maybe l) ->
--   (r -> δr -> Maybe r) ->
--   (l, r) ->
--   Diff δl δr ->
--   Maybe (l, r)
-- patchMaybe = patch
