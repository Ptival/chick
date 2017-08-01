module Script where

import Text.PrettyPrint.Annotated.WL

import PrettyPrinting.PrettyPrintableUnannotated
import Vernacular

data Script α ν = Script [Vernacular α ν]
  deriving (Show)

instance PrettyPrintableUnannotated (Script α) where
  prettyDocU (Script s) = vsep <$> mapM prettyDocU s
