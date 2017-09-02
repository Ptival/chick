{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Typing.GlobalDeclaration
  ( GlobalDeclaration(..)
  , nameOf
  )where

import Control.Monad.Reader
import Data.Default
import Text.PrettyPrint.Annotated.WL

import Inductive.Inductive
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import Term.Term
import Term.Variable

data GlobalDeclaration α ν
  = GlobalAssum ν (TypeX α ν)
  -- careful, the term and type arguments are convertible
  -- v : τ := t
  | GlobalDef
    { globalDefName :: ν
    , globalDefType :: TypeX α ν
    , globalDefDefn :: TermX α ν
    }
  | GlobalInd   (Inductive α ν)

deriving instance Eq (GlobalDeclaration α Variable)
deriving instance (Show α, Show ν) => Show (GlobalDeclaration α ν)

instance
  PrettyPrintableUnannotated (TermX α Variable) =>
  PrettyPrintableUnannotated (GlobalDeclaration α Variable) where
  prettyDocU = \case
    GlobalAssum (Variable v) τ -> do
      τDoc <- prettyDocU τ
      return $ fillSep
        [ text v
        , char ':'
        , τDoc
        ]
    GlobalDef (Variable v) τ t -> do
      τDoc <- prettyDocU τ
      tDoc <- prettyDocU t
      return $ fillSep
        [ text v
        , char ':'
        , τDoc
        , text ":="
        , tDoc
        ]
    GlobalInd i -> prettyDocU i

instance
  PrettyPrintable (TermX α Variable) =>
  PrettyPrintable (GlobalDeclaration α Variable) where
  prettyDoc d = runReader (prettyDocU d) def

nameOf :: GlobalDeclaration α ν -> ν
nameOf (GlobalAssum v _) = v
nameOf (GlobalDef v _ _) = v
nameOf (GlobalInd (Inductive v _ _ _)) = v
