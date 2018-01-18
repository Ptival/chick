{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module DefinitionObjectKind
  ( DefinitionObjectKind(..)
  ) where

import Control.Monad.Reader (runReader)
import Data.Default (def)
import Text.PrettyPrint.Annotated.WL (text)

import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated

data DefinitionObjectKind
  = Definition
  | Fixpoint
  deriving (Eq, Show)

instance PrettyPrintable DefinitionObjectKind where
  prettyDoc v = runReader (prettyDocU v) def

instance PrettyPrintableUnannotated DefinitionObjectKind where
  prettyDocU = \case
    Definition -> return $ text "Definition" 
    Fixpoint   -> return $ text "Fixpoint" 

