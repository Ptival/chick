{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language TypeSynonymInstances #-}
{-# language UndecidableInstances #-}

module Term.TypeErroredTerm where

import Control.Monad.Identity

import DictMeta
import DictMetaId
import DictMetaMap
import DictMetaMapId
import Term.RawTerm
import Term.Term
import Term.TypeCheckedTerm
import TypeCheckingFailure

data TypeErrored

type TypeErroredTerm = TermX TypeErrored
type TypeErroredType = TypeErroredTerm

type TypeError tChecked = Either (TypeCheckingFailure RawTerm) tChecked

type instance X_Annot TypeErrored = TypeError (X_Annot TypeChecked)
type instance X_App   TypeErrored = TypeError (X_App   TypeChecked)
type instance X_Hole  TypeErrored = TypeError (X_Hole  TypeChecked)
type instance X_Lam   TypeErrored = TypeError (X_Lam   TypeChecked)
type instance X_Let   TypeErrored = TypeError (X_Let   TypeChecked)
type instance X_Pi    TypeErrored = TypeError (X_Pi    TypeChecked)
type instance X_Type  TypeErrored = TypeError (X_Type  TypeChecked)
type instance X_Var   TypeErrored = TypeError (X_Var   TypeChecked)

uncheckedDict :: DictMetaMapId ξ TypeErrored
uncheckedDict = DictMetaMap f f f f f f f f
  where
    f :: a -> Identity (TypeError b)
    f _ = pure (Left Unchecked)

unchecked :: TermX ξ -> TypeErroredTerm
unchecked = metaMapId uncheckedDict

fromCheckedDict :: DictMetaMapId TypeChecked TypeErrored
fromCheckedDict =
  DictMetaMap f f f f f f f f
  where
    f :: Identity a -> Identity (TypeError a)
    f = fmap Right

fromChecked :: TypeCheckedTerm -> TypeErroredTerm
fromChecked = metaMapId fromCheckedDict

{-
`annotateError e term` marks the entire `term` unchecked, and marks its head
with the error `e`
-}
annotateError :: TypeCheckingFailure RawTerm -> TermX ξ -> TypeErroredTerm
annotateError e term =
  metaIdHead (DictMeta x x x x x x x x) $ unchecked term
  where
    x :: Identity (TypeError a)
    x = pure (Left e)
