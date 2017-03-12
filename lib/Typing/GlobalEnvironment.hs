{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Typing.GlobalEnvironment where

import           Control.Applicative

import           Inductive.Constructor
import           Inductive.Inductive
import           Term.Term
import           Term.TypeChecked      as TypeChecked
import qualified Typing.LocalContext   as LocalContext

data GlobalDeclaration ξ
  = GlobalAssum Variable (TypeX ξ)
  | GlobalDef   Variable (TermX ξ) (TypeX ξ)
  | GlobalInd   (Inductive ξ)

deriving instance (ForallX Eq   ξ) => Eq   (GlobalDeclaration ξ)
deriving instance (ForallX Show ξ) => Show (GlobalDeclaration ξ)

nameOf :: GlobalDeclaration ξ -> Variable
nameOf (GlobalAssum v _) = v
nameOf (GlobalDef v _ _) = v
nameOf (GlobalInd (Inductive v _ _ _)) = v

type GlobalEnvironment ξ = [GlobalDeclaration ξ]

addGlobalAssum :: (Binder, TypeX ξ) -> GlobalEnvironment ξ -> GlobalEnvironment ξ
addGlobalAssum (Binder Nothing , _) γ = γ
addGlobalAssum (Binder (Just v), τ) γ = (GlobalAssum v τ) : γ

type TypeCheckedGlobalEnvironment = GlobalEnvironment TypeChecked

{-
An inductive brings into scope:
- the inductive type it defines
- inductive definitions for its constructors
TODO: - recursors
-}
unwrapInductive :: Inductive TypeChecked -> [GlobalDeclaration TypeChecked]
unwrapInductive (Inductive v ps is cs) =
  GlobalAssum v (inductiveType ps is (Type ())) : map constructorAssumption cs
  where
    constructorAssumption (Constructor cv cps cis) =
      GlobalAssum cv (constructorType v cps cis)

lookupType :: Variable -> GlobalEnvironment TypeChecked -> Maybe (TypeChecked.Type)
lookupType _ [] = Nothing
lookupType target (d : ds) = lookupIn d <|> lookupType target ds
  where
    lookupIn = \case
      GlobalAssum v τ   -> if v == target then Just τ else Nothing
      GlobalDef   v _ τ -> if v == target then Just τ else Nothing
      GlobalInd   i     -> lookupType target (unwrapInductive i)

toLocalContext ::
  GlobalEnvironment TypeChecked -> LocalContext.LocalContext TypeChecked
toLocalContext = foldr (\ g acc -> localize g ++ acc) []
  where
    localize ::
      GlobalDeclaration TypeChecked -> [LocalContext.LocalDeclaration TypeChecked]
    localize = \case
      GlobalAssum v τ   -> [LocalContext.LocalAssum v τ]
      GlobalDef   v t τ -> [LocalContext.LocalDef v t τ]
      GlobalInd   i     -> concatMap localize (unwrapInductive i)
