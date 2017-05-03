{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Inductive.Constructor where

import Term.Binder
import Term.Term
import Term.Variable
import Term.TypeChecked as TypeChecked

data Constructor ξ =
  Constructor
  { name       :: Variable
  , parameters :: [(Binder, TypeX ξ)]
  , indices    :: [TypeX ξ]
  }

deriving instance (ForallX Eq   ξ) => Eq   (Constructor ξ)
deriving instance (ForallX Show ξ) => Show (Constructor ξ)

constructorTypeUnchecked :: forall ξ.
  X_App ξ -> X_Hole ξ -> X_Pi ξ -> X_Var ξ ->
  Variable -> [(Binder, TypeX ξ)] ->
  [(Binder, TypeX ξ)] -> [TypeX ξ] ->
  TypeX ξ
constructorTypeUnchecked appA holeA piA varA ind indps ps is =
  foldr onParam (
    foldr onIndex (
        foldr onIndParam
        (Var varA ind)
        indps)
    is)
  ps
  where
    onIndex :: TypeX ξ -> TypeX ξ -> TypeX ξ
    onIndex i t = App appA t i
    onParam :: (Binder, TypeX ξ) -> TypeX ξ -> TypeX ξ
    onParam (b, p) t = Pi piA b p t
    onIndParam :: (Binder, TypeX ξ) -> TypeX ξ -> TypeX ξ
    onIndParam (Binder (Just v), τ) t = App appA t (Var varA v)
    onIndParam (Binder Nothing,  τ) t = App appA t (Hole holeA)

constructorTypeChecked ::
  Variable -> [(Binder, TypeChecked.Type)] ->
  [(Binder, TypeChecked.Type)] -> [TypeChecked.Type] ->
  TypeChecked.Type
constructorTypeChecked ind indps ps is =
  foldr onParam (
    foldr onIndex (
        foldr onIndParam
        (Var (Type ()) ind)
        indps)
    is)
  ps
  where
    onIndex :: TypeChecked.Type -> TypeChecked.Type -> TypeChecked.Type
    onIndex i t = App (Type ()) t i
    onParam :: (Binder, TypeChecked.Type) -> TypeChecked.Type -> TypeChecked.Type
    onParam (b, p) t = Pi (Type ()) b p t
    onIndParam :: (Binder, TypeChecked.Type) -> TypeChecked.Type -> TypeChecked.Type
    onIndParam (Binder (Just v), τ) t = App (Type ()) t (Var τ v)
    onIndParam (Binder Nothing,  τ) t = App (Type ()) t (Hole τ)
