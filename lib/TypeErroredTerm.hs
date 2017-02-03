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

instance ConvertTo TypeError () where
  convert () = Right Unchecked

unchecked :: ForallX (ConvertTo TypeError) ξ =>
            TermX ξ -> TypeErroredTerm
unchecked = convertMap

instance ConvertTo TypeError TypeAnnotation where
  convert = Left

fromChecked :: TypeCheckedTerm -> TypeErroredTerm
fromChecked = convertMap

{-
`annotateError e term` marks the entire `term` unchecked, and marks its head
with the error `e`
-}
annotateError :: ForallX (ConvertTo TypeError) ξ =>
                TypeError -> TermX ξ -> TypeErroredTerm
annotateError e term = case unchecked term of
  Annot _ t τ     -> Annot e t τ
  App   _ t1 t2   -> App   e t1 t2
  Hole  _         -> Hole  e
  Lam   _ n t     -> Lam   e n t
  Let   _ n t1 t2 -> Let   e n t1 t2
  Pi    _ n τ1 τ2 -> Pi    e n τ1 τ2
  Type  _         -> Type  e
  Var   _ x       -> Var   e x
