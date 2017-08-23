{-# LANGUAGE FlexibleInstances #-}

module Script where

import Text.PrettyPrint.Annotated.WL

import PrettyPrinting.PrettyPrintableUnannotated
import Term.Variable
import Vernacular

data Script α ν = Script [Vernacular α ν]
  deriving (Eq, Show)

instance PrettyPrintableUnannotated (Script α Variable) where
  prettyDocU (Script s) = encloseSep lbracket rbracket comma <$> mapM prettyDocU s
