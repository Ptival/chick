{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Typing.GlobalDeclaration where

import Inductive.Inductive
import Substitutable
import Term.Term
import Term.Variable

data GlobalDeclaration ξ
  = GlobalAssum Variable (TypeX ξ)
  -- careful, the term and type arguments are convertible
  -- v := t : τ
  | GlobalDef   Variable (TermX ξ) (TypeX ξ)
  | GlobalInd   (Inductive ξ)

deriving instance (ForallX Eq   ξ) => Eq   (GlobalDeclaration ξ)
deriving instance (ForallX Show ξ) => Show (GlobalDeclaration ξ)

nameOf :: GlobalDeclaration ξ -> Variable
nameOf (GlobalAssum v _) = v
nameOf (GlobalDef v _ _) = v
nameOf (GlobalInd (Inductive v _ _ _)) = v

instance Substitutable GlobalDeclaration where
  subst target replacement = \case
    GlobalAssum v τ   -> GlobalAssum v (s τ)
    GlobalDef   v t τ -> GlobalDef   v (s t) (s τ)
    GlobalInd   i     -> GlobalInd   i
    where
      s = subst target replacement
