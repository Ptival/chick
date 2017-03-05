{-# language LambdaCase #-}
{-# language TypeFamilies #-}

module Term.TypeCheckedTerm where

import Term.Term

data TypeChecked

type TypeCheckedTerm = TermX TypeChecked

type instance X_Annot TypeChecked = ()
type instance X_App   TypeChecked = TypeCheckedTerm
type instance X_Hole  TypeChecked = TypeCheckedTerm
type instance X_Lam   TypeChecked = TypeCheckedTerm
type instance X_Let   TypeChecked = TypeCheckedTerm
type instance X_Pi    TypeChecked = TypeCheckedTerm
type instance X_Type  TypeChecked = () -- eventually, universe information
type instance X_Var   TypeChecked = TypeCheckedTerm

typeOf :: TypeCheckedTerm -> TypeCheckedTerm
typeOf = \case
  Annot () _ τ   -> τ
  App   a _ _   -> a
  Hole  a       -> a
  Lam   a _ _   -> a
  Let   a _ _ _ -> a
  Pi    a _ _ _ -> a
  Type  ()       -> Type ()
  Var   a _     -> a
