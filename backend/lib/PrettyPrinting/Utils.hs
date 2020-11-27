module PrettyPrinting.Utils where

import Prettyprinter
import Prettyprinter.Render.String

import Precedence

doc2String :: Doc a -> String
doc2String = renderString . layoutPretty defaultLayoutOptions

par :: PrecedenceTable -> (Precedence, Tolerance) -> (Doc a, Precedence) -> Doc a
par precs (pOut, t) (d, pIn) =
  if isTolerable (tableToOrdering precs) pIn (pOut, t)
  then d
  else parens . nest 2 $ d
