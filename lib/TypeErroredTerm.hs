{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language TypeSynonymInstances #-}

module TypeErroredTerm where

import RawTerm
import Term
import TypeCheckedTerm
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
unchecked = meta' (Right Unchecked)

fromChecked :: TypeCheckedTerm -> TypeErroredTerm
fromChecked = metaMap' Left

{-
`annotateError e term` marks the entire `term` unchecked, and marks its head
with the error `e`
-}
annotateError :: TypeError -> TermX ξ -> TypeErroredTerm
annotateError e term =
  metaHead' e $ unchecked term
