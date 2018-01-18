{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Script where

import Text.PrettyPrint.Annotated.WL

import PrettyPrinting.PrettyPrintableUnannotated
import Term.Variable
import Vernacular

data Script α ν = Script [Vernacular α ν]
  deriving (Show)

deriving instance (Eq α) => Eq (Script α Variable)

instance PrettyPrintableUnannotated (Script α Variable) where
  prettyDocU (Script s) = vsep . punctuate line <$> mapM prettyDocU s
