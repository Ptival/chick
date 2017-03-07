{-# language LambdaCase #-}
{-# language TypeFamilies #-}

module Term.TypeChecked where

import Term.Term

data TypeChecked

type Term = TermX TypeChecked
type Type = Term

type instance X_Annot TypeChecked = ()
type instance X_App   TypeChecked = Term
type instance X_Hole  TypeChecked = Term
type instance X_Lam   TypeChecked = Term
type instance X_Let   TypeChecked = Term
type instance X_Pi    TypeChecked = Term
type instance X_Type  TypeChecked = () -- eventually, universe information
type instance X_Var   TypeChecked = Term

typeOf :: Term -> Term
typeOf = \case
  Annot () _ τ   -> τ
  App   a _ _   -> a
  Hole  a       -> a
  Lam   a _ _   -> a
  Let   a _ _ _ -> a
  Pi    a _ _ _ -> a
  Type  ()       -> Type ()
  Var   a _     -> a
