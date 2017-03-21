{-# language LambdaCase #-}

module PrettyPrinting.GlobalEnvironment where

import Text.PrettyPrint.Annotated.WL

import DictMetaOut
import Precedence
import PrettyPrinting.Inductive
import PrettyPrinting.Term
import PrettyPrinting.Utils
import Term.Term
import Term.TypeChecked              as TypeChecked
import Typing.GlobalEnvironment

prettyGlobalDeclarationDoc ::
  DictMetaOut a TypeChecked -> PrecedenceTable -> GlobalDeclaration TypeChecked -> Doc a
prettyGlobalDeclarationDoc dict precs = \case
  GlobalAssum (Variable v) τ ->
    fillSep
    [ text v
    , char ':'
    , prettyTermDoc dict precs τ
    ]
  GlobalDef (Variable v) t τ ->
    fillSep
    [ text v
    , text ":="
    , prettyTermDoc dict precs t
    , char ':'
    , prettyTermDoc dict precs τ
    ]
  GlobalInd ind -> prettyInductiveDoc dict precs ind

prettyGlobalEnvironment :: GlobalEnvironment TypeChecked -> String
prettyGlobalEnvironment =
  doc2String . prettyGlobalEnvironmentDoc ignoreAnnotations def

prettyGlobalEnvironmentDoc ::
  DictMetaOut a TypeChecked -> PrecedenceTable -> GlobalEnvironment TypeChecked -> Doc a
prettyGlobalEnvironmentDoc dict precs ge =
  vsep (map (prettyGlobalDeclarationDoc dict precs) (reverse ge))
