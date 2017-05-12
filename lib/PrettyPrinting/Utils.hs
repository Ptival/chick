module PrettyPrinting.Utils where

import Text.PrettyPrint.Annotated.WL

import Precedence

doc2String :: Doc a -> String
doc2String = display . renderPretty 1.0 80

par :: PrecedenceTable -> (Precedence, Tolerance) -> (Doc a, Precedence) -> Doc a
par precs (pOut, t) (d, pIn) =
  if isTolerable (tableToOrdering precs) pIn (pOut, t)
  then d
  else parens . nest 2 $ d
