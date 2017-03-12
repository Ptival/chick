{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Typing.LocalContext where

import Control.Applicative
import Test.QuickCheck

import Term.Term
import Term.TypeChecked

data LocalDeclaration ξ
  = LocalAssum Variable (TypeX ξ)
  | LocalDef   Variable (TermX ξ) (TypeX ξ)

deriving instance (ForallX Eq   ξ) => Eq   (LocalDeclaration ξ)
deriving instance (ForallX Show ξ) => Show (LocalDeclaration ξ)

instance (ForallX Arbitrary ξ) => Arbitrary (LocalDeclaration ξ) where
  arbitrary =
    oneof
    [ LocalAssum <$> arbitrary <*> genTerm 2
    , LocalDef   <$> arbitrary <*> genTerm 2 <*> genTerm 2
    ]

nameOf :: LocalDeclaration ξ -> Variable
nameOf (LocalAssum v _)   = v
nameOf (LocalDef   v _ _) = v

type LocalContext ξ = [LocalDeclaration ξ]

addLocalAssum :: (Binder, TypeX ξ) -> LocalContext ξ -> LocalContext ξ
addLocalAssum (Binder Nothing , _) γ = γ
addLocalAssum (Binder (Just v), τ) γ = (LocalAssum v τ) : γ

type TypeCheckedLocalContext = LocalContext TypeChecked

lookupType :: Variable -> LocalContext ξ -> Maybe (TypeX ξ)
lookupType _ [] = Nothing
lookupType target (d : ds) = lookupIn d <|> lookupType target ds
  where
    lookupIn = \case
      LocalAssum v τ   -> if v == target then Just τ else Nothing
      LocalDef   v _ τ -> if v == target then Just τ else Nothing
