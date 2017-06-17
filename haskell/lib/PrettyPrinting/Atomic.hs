{-# language LambdaCase #-}

module PrettyPrinting.Atomic where

import Text.PrettyPrint.Annotated.WL

import PrettyPrinting.PrettyPrintable

import Tactic

prettyAtomicDoc :: PrettyPrintable ν => Atomic ν -> Doc a
prettyAtomicDoc = \case
  Admit    -> text "admit"
  Destruct -> text "destruct"
  Exact v  -> fillSep [text "exact", prettyDoc v]
  Intro b  -> fillSep [text "intro", prettyDoc b]
