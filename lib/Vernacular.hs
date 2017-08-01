{-# LANGUAGE LambdaCase #-}

module Vernacular
  ( Vernacular(..)
  ) where

import           Text.PrettyPrint.Annotated.WL

import qualified Inductive.Inductive as I
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           Term.Term

data Vernacular α ν
  = Definition ν (TypeX α ν) (TermX α ν)
  | Inductive (I.Inductive α ν)
  deriving (Show)

instance PrettyPrintableUnannotated (Vernacular α) where
  prettyDocU = \case

    Definition n τ t -> do
      let nDoc = prettyDoc n
      τDoc <- prettyDocU τ
      tDoc <- prettyDocU t
      return $ fillSep
        [ text "Definition"
        , nDoc
        , text ":"
        , τDoc
        , text ":="
        , tDoc
        ]

    Inductive ind -> do
      prettyDocU ind
