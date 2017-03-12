{-# language FlexibleContexts #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Inductive.Constructor where

import Term.Term
import Term.TypeChecked as TypeChecked

data Constructor ξ =
  Constructor
  { name      :: Variable
  , arguments :: [(Binder, TypeX ξ)]
  , indices   :: [TypeX ξ]
  }

deriving instance (ForallX Eq   ξ) => Eq   (Constructor ξ)
deriving instance (ForallX Show ξ) => Show (Constructor ξ)

constructorType ::
  Variable -> [(Binder, TypeChecked.Type)] ->
  [(Binder, TypeChecked.Type)] -> [TypeChecked.Type] ->
  TypeChecked.Type
constructorType ind indps ps is =
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
