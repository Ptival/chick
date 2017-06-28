{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Typing.GlobalDeclaration
  ( GlobalDeclaration(..)
  , nameOf
  )where

import Text.PrettyPrint.Annotated.WL

import Inductive.Inductive
import PrettyPrinting.PrettyPrintableUnannotated
import Term.Term
import Term.Variable

data GlobalDeclaration ξ ν
  = GlobalAssum ν (TypeX ξ ν)
  -- careful, the term and type arguments are convertible
  -- v := t : τ
  | GlobalDef   ν (TermX ξ ν) (TypeX ξ ν)
  | GlobalInd   (Inductive ξ ν)

deriving instance (Eq ξ, Eq ν) => Eq (GlobalDeclaration ξ ν)
deriving instance (Show ξ, Show ν) => Show (GlobalDeclaration ξ ν)

instance
  PrettyPrintableUnannotated (TermX ξ) =>
  PrettyPrintableUnannotated (GlobalDeclaration ξ) where
  prettyDocU = \case
    GlobalAssum (Variable v) τ -> do
      τDoc <- prettyDocU τ
      return $ fillSep
        [ text v
        , char ':'
        , τDoc
        ]
    GlobalDef (Variable v) t τ -> do
      tDoc <- prettyDocU t
      τDoc <- prettyDocU τ
      return $ fillSep
        [ text v
        , text ":="
        , tDoc
        , char ':'
        , τDoc
        ]
    GlobalInd i -> prettyDocU i

nameOf :: GlobalDeclaration ξ ν -> ν
nameOf (GlobalAssum v _) = v
nameOf (GlobalDef v _ _) = v
nameOf (GlobalInd (Inductive v _ _ _)) = v

{-
instance Substitutable GlobalDeclaration where
  subst target replacement = \case
    GlobalAssum v τ   -> GlobalAssum v (s τ)
    GlobalDef   v t τ -> GlobalDef   v (s t) (s τ)
    GlobalInd   i     -> GlobalInd   i
    where
      s = subst target replacement
-}
