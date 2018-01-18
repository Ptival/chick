{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Definition
  ( Definition(..)
  ) where

import           Control.Monad.Reader
import           Data.Default
import           Text.PrettyPrint.Annotated.WL

import           DefinitionObjectKind (DefinitionObjectKind)
import           PrettyPrinting.Term ()
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           Term.Term

data Definition α ν = Definition
  { definitionKind :: DefinitionObjectKind
  , definitionName :: ν
  , definitionType :: TypeX α ν
  , definitionTerm :: TermX α ν
  }
  deriving (Show)

deriving instance (Eq α) => Eq (Definition α Variable)

instance PrettyPrintable (Definition α Variable) where
  prettyDoc v = runReader (prettyDocU v) def

instance PrettyPrintableUnannotated (Definition α Variable) where
  prettyDocU d = do
    τDoc <- prettyDocU $ definitionType d
    tDoc <- prettyDocU $ definitionTerm d
    return $ hcat
      [ prettyDoc (definitionKind d)
      , space
      , prettyDoc (definitionName d)
      , softline
      , text ":"
      , space
      , τDoc
      , softline
      , text ":="
      , softline
      , tDoc
      ]
