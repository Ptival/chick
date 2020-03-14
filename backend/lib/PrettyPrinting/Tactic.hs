
module PrettyPrinting.Tactic where

{-

import Text.PrettyPrint.Annotated.WL

import PrettyPrinting.Atomic
import Tactic
import Term.Variable

prettyTacticDoc :: Tactic Variable -> Doc a
prettyTacticDoc = \case
  Atomic a -> prettyAtomicDoc a
  Semicolon a b ->
    fillCat
    [ prettyTacticDoc a
    , text "; "
    , prettyTacticDoc b
    ]
-}
