{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff.Binder where

import           Data.String
import qualified Diff.Atom as DA
import           Term.Binder

fromΔVariable :: DA.Diff ν -> DA.Diff (Binder ν)
fromΔVariable = \case
  DA.Same      -> DA.Same
  DA.Replace v -> DA.Replace (Binder (Just v))

toΔVariable :: IsString ν => DA.Diff (Binder ν) -> DA.Diff ν
toΔVariable = \case
  DA.Same                      -> DA.Same
  DA.Replace (Binder Nothing)  -> DA.Replace "__?__"
  DA.Replace (Binder (Just v)) -> DA.Replace v
