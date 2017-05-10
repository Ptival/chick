{-# language LambdaCase #-}
{-# language TypeFamilies #-}

module Term.TypeChecked where

import Term.Term

data TypeChecked ν

type Term ν = TermX (TypeChecked ν) ν
type Type ν = Term ν

type instance X_Annot (TypeChecked ν) = ()
type instance X_App   (TypeChecked ν) = Term ν
type instance X_Hole  (TypeChecked ν) = Term ν
type instance X_Lam   (TypeChecked ν) = Term ν
type instance X_Let   (TypeChecked ν) = Term ν
type instance X_Pi    (TypeChecked ν) = Term ν
type instance X_Type  (TypeChecked ν) = () -- eventually, universe information
type instance X_Var   (TypeChecked ν) = Term ν

typeOf :: Term ν -> Term ν
typeOf = \case
  Annot () _ τ -> τ
  App   a _ _ -> a
  Hole  a     -> a
  Lam   a _   -> a
  Let   a _ _ -> a
  Pi    a _ _ -> a
  Type  ()     -> Type ()
  Var     _   -> error "removed annotations from Var"
