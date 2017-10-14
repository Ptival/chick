{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

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

data Vernacular α ν
  = Definition
    { definitionIsFixpoint :: Bool
    , definitionName :: ν
    , definitionType :: TypeX α ν
    , definitionTerm :: TermX α ν
    }
  | Inductive (I.Inductive α ν)
  deriving (Show)

deriving instance Eq (Vernacular α Variable)

instance PrettyPrintable (Vernacular α Variable) where
  prettyDoc v = runReader (prettyDocU v) def

instance PrettyPrintableUnannotated (Vernacular α Variable) where
  prettyDocU = \case

    Definition b n τ t -> do
      let nDoc = prettyDoc n
      τDoc <- prettyDocU τ
      tDoc <- prettyDocU t
      return $ hcat
        [ text (if b then "Fixpoint" else "Definition")
        , space
        , nDoc
        , softline
        , text ":"
        , space
        , τDoc
        , softline
        , text ":="
        , softline
        , tDoc
        , text "."
        ]

    Inductive ind -> do
      indDoc <- prettyDocU ind
      return $ hcat
        [ indDoc
        , text "."
        ]
