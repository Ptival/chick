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
import Term.Raw         as Raw

data Constructor ξ ν =
  Constructor
  { name       :: ν
  , parameters :: [(Binder ν, TypeX ξ ν)]
  , indices    :: [TypeX ξ ν]
  }

deriving instance (Eq ξ, Eq ν) => Eq (Constructor ξ ν)
deriving instance (Show ξ, Show ν) => Show (Constructor ξ ν)

rawConstructorType ::
  Variable -> [(Binder Variable, TermX ξ Variable)] ->
  [(Binder Variable, TermX ξ Variable)] -> [TermX ξ Variable] ->
  TypeX () Variable
rawConstructorType ind indps ps is =
  foldr onParam (
    foldr onIndex (
        foldr onIndParam
        (Var (Just ()) ind)
        indps)
    is)
  ps
  where
    onIndex :: TermX ξ Variable -> Raw.Term Variable -> Raw.Term Variable
    onIndex i t = App () (raw t) (raw i)
    onParam :: (Binder Variable, TermX ξ Variable) -> Raw.Term Variable -> Raw.Term Variable
    onParam (b, p) t = Pi () (raw p) (abstractBinder b t)
    onIndParam :: (Binder Variable, TermX ξ Variable) -> Raw.Term Variable -> Raw.Term Variable
    onIndParam (Binder (Just v), _) t = App () (raw t) (Var (Just ()) v)
    onIndParam (Binder Nothing,  _) t = App () (raw t) (Hole ())

constructorTypeChecked ::
  Variable -> [(Binder Variable, TypeChecked.Type Variable)] ->
  [(Binder Variable, TypeChecked.Type Variable)] -> [TypeChecked.Type Variable] ->
  TypeChecked.Type Variable
constructorTypeChecked ind indps ps is =
  foldr onParam (
    foldr onIndex (
        foldr onIndParam
        (Var Nothing ind) -- TODO: this Nothing could be replace by the right Type -> ... -> Type
        indps)
    is)
  ps
  where
    onIndex :: TypeChecked.Type Variable -> TypeChecked.Type Variable -> TypeChecked.Type Variable
    onIndex i t = App (Checked Type) t i
    onParam :: (Binder Variable, TypeChecked.Type Variable) -> TypeChecked.Type Variable -> TypeChecked.Type Variable
    onParam (b, p) t = Pi (Checked Type) p (abstractBinder b t)
    onIndParam :: (Binder Variable, TypeChecked.Type Variable) -> TypeChecked.Type Variable -> TypeChecked.Type Variable
    onIndParam (Binder (Just v), _) t = App (Checked Type) t (Var Nothing v)
    onIndParam (Binder Nothing,  τ) t = App (Checked Type) t (Hole (Checked τ))

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
    onIndex i t = App Type t i
    onParam :: (Binder Variable, TypeChecked.Type Variable) -> TypeChecked.Type Variable -> TypeChecked.Type Variable
    onParam (b, p) t = Pi Type p (abstractBinder b t)
    onIndParam :: (Binder Variable, TypeChecked.Type Variable) -> TypeChecked.Type Variable -> TypeChecked.Type Variable
    onIndParam (Binder (Just v), τ) t = App Type t (Var v)
    onIndParam (Binder Nothing,  τ) t = App Type t (Hole τ)
-}
