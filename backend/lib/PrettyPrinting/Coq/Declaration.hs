{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module PrettyPrinting.Coq.Declaration
  ( prettyDocU
  ) where

import Text.PrettyPrint.Annotated.WL

import Inductive.Inductive
import Language
import PrettyPrinting.PrettyPrintableUnannotated
import Vernacular

instance PrettyPrintable (Vernacular α Variable) where
  prettyDoc v = runReader (prettyDocU v) def

instance PrettyPrintableUnannotated OCaml (Vernacular α Variable) where
  prettyStrU = \case
    Definition defn -> do
    defDoc <- prettyDocU target defn
    return $ hcat
        [ defDoc
        , text "."
        ]

    Inductive ind -> do
    indDoc <- prettyDocU target ind
    return $ hcat
        [ indDoc
        , text "."
        ]

    Unsupported s -> do
    return $ text s
