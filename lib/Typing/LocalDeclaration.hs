{-# LANGUAGE DeriveFoldable #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Typing.LocalDeclaration where

import Control.Monad.Reader
import Data.Default
import Text.PrettyPrint.Annotated.WL

import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import Term.Binder
import Term.Term
import Term.Variable

data LocalDeclaration α ν
  = LocalAssum (Binder ν) (TypeX α ν)
  | LocalDef
    { localDefName :: ν
    , localDefType :: TypeX α ν
    , localDefTerm :: TermX α ν
    }

deriving instance (Eq α, Eq ν) => Eq (LocalDeclaration α ν)
deriving instance (Show α, Show ν) => Show (LocalDeclaration α ν)

instance
  PrettyPrintableUnannotated (TermX α Variable) =>
  PrettyPrintable (LocalDeclaration α Variable) where
  prettyDoc d = runReader (prettyDocU d) def

instance
  PrettyPrintableUnannotated (TermX α Variable) =>
  PrettyPrintableUnannotated (LocalDeclaration α Variable) where
  prettyDocU = \case
    LocalAssum b τ -> do
      τDoc <- prettyDocU τ
      return $ fillSep
        [ prettyDoc b
        , char ':'
        , τDoc
        ]
    LocalDef (Variable v) τ t -> do
      τDoc <- prettyDocU τ
      tDoc <- prettyDocU t
      return $ fillSep
        [ text v
        , char ':'
        , τDoc
        , text ":="
        , tDoc
        ]

nameOf :: LocalDeclaration α ν -> Maybe ν
nameOf (LocalAssum b _)   = unBinder b
nameOf (LocalDef   v _ _) = Just v

typeOf :: LocalDeclaration α ν -> TypeX α ν
typeOf (LocalAssum _ τ  ) = τ
typeOf (LocalDef   _ τ _) = τ
