module PrettyPrinting.Utils where

import Precedence
  ( Precedence,
    PrecedenceTable,
    Tolerance,
    isTolerable,
    tableToOrdering,
  )
import Prettyprinter
  ( Doc,
    defaultLayoutOptions,
    layoutPretty,
    nest,
    parens,
  )
import Prettyprinter.Render.String (renderString)

doc2String :: Doc a -> String
doc2String = renderString . layoutPretty defaultLayoutOptions

par :: PrecedenceTable -> (Precedence, Tolerance) -> (Doc a, Precedence) -> Doc a
par precs (pOut, t) (d, pIn) =
  if isTolerable (tableToOrdering precs) pIn (pOut, t)
    then d
    else parens . nest 2 $ d
