{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Inductive.Constructor where

import Data.Default

import Term.Binder
import Term.Term
import Term.Variable
import Term.TypeChecked as TypeChecked

data Constructor ξ ν =
  Constructor
  { name       :: ν
  , parameters :: [(Binder ν, TypeX ξ ν)]
  , indices    :: [TypeX ξ ν]
  }

deriving instance (ForallX Eq ξ, Eq ν) => Eq (Constructor ξ ν)
deriving instance (ForallX Show ξ, Show ν) => Show (Constructor ξ ν)

constructorTypeUnchecked :: forall ξ.
  (Default (X_App ξ), Default (X_Hole ξ), Default (X_Pi ξ)) =>
  Variable -> [(Binder Variable, TypeX ξ Variable)] ->
  [(Binder Variable, TypeX ξ Variable)] -> [TypeX ξ Variable] ->
  TypeX ξ Variable
constructorTypeUnchecked ind indps ps is =
  foldr onParam (
    foldr onIndex (
        foldr onIndParam
        (Var ind)
        indps)
    is)
  ps
  where
    onIndex :: TypeX ξ Variable -> TypeX ξ Variable -> TypeX ξ Variable
    onIndex i t = App def t i
    onParam :: (Binder Variable, TypeX ξ Variable) -> TypeX ξ Variable -> TypeX ξ Variable
    onParam (b, p) t = Pi def p (abstractBinder b t)
    onIndParam :: (Binder Variable, TypeX ξ Variable) -> TypeX ξ Variable -> TypeX ξ Variable
    onIndParam (Binder (Just v), _) t = App def t (Var v)
    onIndParam (Binder Nothing,  _) t = App def t (Hole def)

constructorTypeChecked ::
  Variable -> [(Binder Variable, TypeChecked.Type Variable)] ->
  [(Binder Variable, TypeChecked.Type Variable)] -> [TypeChecked.Type Variable] ->
  TypeChecked.Type Variable
constructorTypeChecked ind indps ps is =
  foldr onParam (
    foldr onIndex (
        foldr onIndParam
        (Var ind)
        indps)
    is)
  ps
  where
    onIndex :: TypeChecked.Type Variable -> TypeChecked.Type Variable -> TypeChecked.Type Variable
    onIndex i t = App (Type ()) t i
    onParam :: (Binder Variable, TypeChecked.Type Variable) -> TypeChecked.Type Variable -> TypeChecked.Type Variable
    onParam (b, p) t = Pi (Type ()) p (abstractBinder b t)
    onIndParam :: (Binder Variable, TypeChecked.Type Variable) -> TypeChecked.Type Variable -> TypeChecked.Type Variable
    onIndParam (Binder (Just v), _) t = App (Type ()) t (Var v)
    onIndParam (Binder Nothing,  τ) t = App (Type ()) t (Hole τ)

{-
This is complicated.

For instance if you have:

T : Type
n : nat
v : Vec T n
H : H T n v
⊢ G T n v

And you destruct v into both constructors, the expected result for cons is:

T : Type
n : nat
xs : Vec T n
H : H T (S n) (vcons n h xs)
⊢ G T (S n) (vcons n h xs)

-}

{-
constructorTerm ::
  Variable -> [(Binder Variable, TypeX ξ Variable)] ->
  [(Binder Variable, TypeX ξ Variable)] -> [TypeX ξ Variable] ->
  Constructor ξ Variable -> TermX ξ Variable
constructorTerm ind indps ips iis (Constructor c ps is) =
  foldr onParam (
    foldr onIndex (
        foldr onIndParam
        (Var ind)
        indps)
    is)
  ps
  where
    onIndex :: TypeChecked.Type Variable -> TypeChecked.Type Variable -> TypeChecked.Type Variable
    onIndex i t = App (Type ()) t i
    onParam :: (Binder Variable, TypeChecked.Type Variable) -> TypeChecked.Type Variable -> TypeChecked.Type Variable
    onParam (b, p) t = Pi (Type ()) p (abstractBinder b t)
    onIndParam :: (Binder Variable, TypeChecked.Type Variable) -> TypeChecked.Type Variable -> TypeChecked.Type Variable
    onIndParam (Binder (Just v), τ) t = App (Type ()) t (Var v)
    onIndParam (Binder Nothing,  τ) t = App (Type ()) t (Hole τ)
-}
