{-# language FlexibleContexts #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Inductive.Inductive where

import Inductive.Constructor
import Term.Term
import Term.TypeChecked      as TypeChecked

data Inductive ξ =
  Inductive
  { name         :: Variable
  , parameters   :: [(Binder, TypeX ξ)]
  , indices      :: [TypeX ξ]
  , constructors :: [Constructor ξ]
  }

deriving instance (ForallX Eq   ξ) => Eq   (Inductive ξ)
deriving instance (ForallX Show ξ) => Show (Inductive ξ)

inductiveType ::
  [(Binder, TypeChecked.Type)] -> [TypeChecked.Type] -> TypeChecked.Type ->
  TypeChecked.Type
inductiveType ps is o =
  foldr onParam (foldr onIndex o is) ps
  where
    onIndex :: TypeChecked.Type -> TypeChecked.Type -> TypeChecked.Type
    onIndex i      t = Pi (Type ()) (Binder Nothing)  i t
    onParam :: (Binder, TypeChecked.Type) -> TypeChecked.Type -> TypeChecked.Type
    onParam (b, p) t = Pi (Type ()) b p t
