module PrettyPrinting.Inductive where

import Text.PrettyPrint.Annotated.WL

import DictMetaOut
import Inductive.Constructor
import Inductive.Inductive
import Precedence
import PrettyPrinting.Variable
import PrettyPrinting.Term
import Term.Term
import Term.TypeChecked              as TypeChecked

arrows :: [Doc a] -> Doc a
arrows = encloseSep mempty mempty (text " →")

prettyInductiveDoc ::
  DictMetaOut a TypeChecked -> PrecedenceTable -> Inductive TypeChecked -> Doc a
prettyInductiveDoc dict precs (Inductive n ps is cs) =
  vsep $
  [ fillSep $
    [ text "inductive"
    , prettyVariableDoc n
    ]
    ++
    (
      -- mempty creates an unwanted space, so have to use []
      if length ps == 0
      then []
      else [encloseSep mempty mempty mempty (map (prettyBindingDoc dict precs) ps)]
    )
    ++
    [ text ":"
    , arrows (map (prettyTermDoc dict precs) is ++ [text "Type"])
    , text "where"
    ]
  ] ++ map (indent 2 . prettyConstructorDoc dict precs n ps) cs

prettyConstructorDoc ::
  DictMetaOut a TypeChecked -> PrecedenceTable ->
  Variable -> [(Binder, TypeChecked.Type)] ->
  Constructor TypeChecked -> Doc a
prettyConstructorDoc dict precs ind indps (Constructor n ps is) =
  fillSep
  [ prettyVariableDoc n
  , text ":"
  , prettyTermDoc dict precs (constructorType ind indps ps is)
    {-
  , arrows (map (prettyBindingDoc dict precs) ps)
  , text "→"
  , prettyVariableDoc ind
  , encloseSep mempty mempty mempty
    (map (par precs (PrecApp, TolerateHigher) . prettyTermDocPrec dict precs) is)
    -}
  ]
