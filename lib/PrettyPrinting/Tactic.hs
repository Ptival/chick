{-# language LambdaCase #-}

module PrettyPrinting.Tactic where

import Text.PrettyPrint.Annotated.WL

import PrettyPrinting.Atomic
import Tactic

prettyTacticDoc :: Tactic -> Doc a
prettyTacticDoc = \case
  Atomic a -> prettyAtomicDoc a
  Semicolon a b ->
    fillCat
    [ prettyTacticDoc a
    , text "; "
    , prettyTacticDoc b
    ]
