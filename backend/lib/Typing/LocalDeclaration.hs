{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Typing.LocalDeclaration where

import Term.Binder
import Term.Term

data LocalDeclaration α ν
  = LocalAssum (Binder ν) (TypeX α ν)
  | LocalDef
    { localDefName :: ν
    , localDefType :: TypeX α ν
    , localDefTerm :: TermX α ν
    }

deriving instance (Eq α, Eq ν) => Eq (LocalDeclaration α ν)
deriving instance (Show α, Show ν) => Show (LocalDeclaration α ν)

nameOf :: LocalDeclaration α ν -> Maybe ν
nameOf (LocalAssum b _)   = unBinder b
nameOf (LocalDef   v _ _) = Just v

typeOf :: LocalDeclaration α ν -> TypeX α ν
typeOf (LocalAssum _ τ  ) = τ
typeOf (LocalDef   _ τ _) = τ
