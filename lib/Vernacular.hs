{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Vernacular
  ( Vernacular(..)
  ) where

import           Control.Monad.Reader
import           Data.Default
import           Text.PrettyPrint.Annotated.WL

import qualified Inductive.Inductive as I
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           Term.Term
import           Term.Variable

data Vernacular α ν
  = Definition ν (TypeX α ν) (TermX α ν)
  | Inductive (I.Inductive α ν)
  deriving (Eq, Show)

instance PrettyPrintable (Vernacular α Variable) where
  prettyDoc v = runReader (prettyDocU v) def

instance PrettyPrintableUnannotated (Vernacular α Variable) where
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
        , text "."
        ]

    Inductive ind -> do
      prettyDocU ind
