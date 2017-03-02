{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language TypeSynonymInstances #-}

module Term.TypeErroredTerm where

import DictMetaId
import DictMetaMapId
import Term.RawTerm
import Term.Term
import Term.TypeCheckedTerm
import TypeCheckingFailure

data TypeErrored

type TypeErroredTerm = TermX TypeErrored
type TypeErroredType = TypeErroredTerm

type TypeError = Either RawType (TypeCheckingFailure RawTerm)

type instance X_Annot TypeErrored = TypeError
type instance X_App   TypeErrored = TypeError
type instance X_Hole  TypeErrored = TypeError
type instance X_Lam   TypeErrored = TypeError
type instance X_Let   TypeErrored = TypeError
type instance X_Pi    TypeErrored = TypeError
type instance X_Type  TypeErrored = TypeError
type instance X_Var   TypeErrored = TypeError

unchecked :: TermX ξ -> TypeErroredTerm
unchecked = metaId' (Right Unchecked)

fromChecked :: TypeCheckedTerm -> TypeErroredTerm
fromChecked = metaMapId' Left

{-
`annotateError e term` marks the entire `term` unchecked, and marks its head
with the error `e`
-}
annotateError :: TypeError -> TermX ξ -> TypeErroredTerm
annotateError e term =
  metaIdHead' e $ unchecked term
