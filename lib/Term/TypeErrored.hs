{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language TypeSynonymInstances #-}
{-# language UndecidableInstances #-}

module Term.TypeErrored where

import Control.Monad.Identity

import           DictMeta
import           DictMetaHead
import           DictMetaId
import           DictMetaMap
import           DictMetaMapId
import qualified Term.Raw            as Raw
import           Term.Term
import qualified Term.TypeChecked    as TypeChecked
import           TypeCheckingFailure

data TypeErrored

type Term = TermX TypeErrored
type Type = Term

type TypeError tChecked = Either (TypeCheckingFailure Raw.Term) tChecked

type instance X_Annot TypeErrored = TypeError (X_Annot TypeChecked.TypeChecked)
type instance X_App   TypeErrored = TypeError (X_App   TypeChecked.TypeChecked)
type instance X_Hole  TypeErrored = TypeError (X_Hole  TypeChecked.TypeChecked)
type instance X_Lam   TypeErrored = TypeError (X_Lam   TypeChecked.TypeChecked)
type instance X_Let   TypeErrored = TypeError (X_Let   TypeChecked.TypeChecked)
type instance X_Pi    TypeErrored = TypeError (X_Pi    TypeChecked.TypeChecked)
type instance X_Type  TypeErrored = TypeError (X_Type  TypeChecked.TypeChecked)
type instance X_Var   TypeErrored = TypeError (X_Var   TypeChecked.TypeChecked)

uncheckedDict :: DictMetaMapId ξ TypeErrored
uncheckedDict = DictMetaMap f f f f f f f f
  where
    f :: a -> Identity (TypeError b)
    f _ = pure (Left Unchecked)

unchecked :: TermX ξ -> Term
unchecked = metaMapId uncheckedDict

fromCheckedDict :: DictMetaMapId TypeChecked.TypeChecked TypeErrored
fromCheckedDict =
  DictMetaMap f f f f f f f f
  where
    f :: Identity a -> Identity (TypeError a)
    f = fmap Right

fromChecked :: TypeChecked.Term -> Term
fromChecked = metaMapId fromCheckedDict

{-
`annotateError e term` marks the entire `term` unchecked, and marks its head
with the error `e`
-}
annotateError :: TypeCheckingFailure Raw.Term -> TermX ξ -> Term
annotateError e term =
  metaIdHead (DictMeta x x x x x x x x) $ unchecked term
  where
    x :: Identity (TypeError a)
    x = pure (Left e)

getTypeError :: Term -> String
getTypeError = metaHead (DictMetaHead f f f f f f f f)
  where
    f :: Show a => a -> String
    f = show
