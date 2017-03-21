{-# language LambdaCase #-}

module PrettyPrinting.LocalContext where

import Text.PrettyPrint.Annotated.WL

import DictMetaOut
import Precedence
import PrettyPrinting.Term
import PrettyPrinting.Utils
import Term.Term
import Typing.LocalContext

prettyLocalDeclarationDoc ::
  DictMetaOut a ξ -> PrecedenceTable -> LocalDeclaration ξ -> Doc a
prettyLocalDeclarationDoc dict precs = \case
  LocalAssum (Variable v) τ ->
    fillSep
    [ text v
    , char ':'
    , prettyTermDoc dict precs τ
    ]
  LocalDef (Variable v) t τ ->
    fillSep
    [ text v
    , text ":="
    , prettyTermDoc dict precs t
    , char ':'
    , prettyTermDoc dict precs τ
    ]

prettyLocalContext :: LocalContext ξ -> String
prettyLocalContext =
  withDefaultTermParser prettyLocalContextDoc

prettyLocalContextDoc ::
  DictMetaOut a ξ -> PrecedenceTable -> LocalContext ξ -> Doc a
prettyLocalContextDoc dict precs ctxt =
  vsep (map (prettyLocalDeclarationDoc dict precs) (reverse ctxt))
