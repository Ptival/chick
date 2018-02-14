{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Vernacular
  ( Vernacular(..)
  ) where

import           Control.Monad.Reader
import           Data.Default
import           Text.PrettyPrint.Annotated.WL

import qualified Definition as D
import qualified Inductive.Inductive as I
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           Term.Term

data Vernacular α ν
  = Definition (D.Definition α ν)
  | Inductive  (I.Inductive α ν)
  deriving (Show)

deriving instance (Eq α) => Eq (Vernacular α Variable)

instance PrettyPrintable (Vernacular α Variable) where
  prettyDoc v = runReader (prettyDocU v) def

instance PrettyPrintableUnannotated (Vernacular α Variable) where
  prettyDocU = \case
    Definition defn -> do
      defDoc <- prettyDocU defn
      return $ hcat
        [ defDoc
        , text "."
        ]

    Inductive ind -> do
      indDoc <- prettyDocU ind
      return $ hcat
        [ indDoc
        , text "."
        ]
