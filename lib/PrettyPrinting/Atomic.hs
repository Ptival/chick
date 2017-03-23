{-# language LambdaCase #-}

module PrettyPrinting.Atomic where

import Text.PrettyPrint.Annotated.WL

import PrettyPrinting.Binder
import PrettyPrinting.Variable
import Tactic

prettyAtomicDoc :: Atomic -> Doc a
prettyAtomicDoc = \case
  Exact v -> fillSep [text "exact", prettyVariableDoc v]
  Intro b -> fillSep [text "intro", prettyBinderDoc b]
