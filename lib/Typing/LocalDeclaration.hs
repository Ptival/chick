{-# LANGUAGE DeriveFoldable #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Typing.LocalDeclaration where

import Test.QuickCheck

import Text.PrettyPrint.Annotated.WL
import PrettyPrinting.PrettyPrintableAnnotated
import Term.Term
import Term.Variable

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

instance
  PrettyPrintableAnnotated TermX =>
  PrettyPrintableAnnotated LocalDeclaration where
  prettyDocA = \case
    LocalAssum (Variable v) τ -> do
      τDoc <- prettyDocA τ
      return $ fillSep
        [ text v
        , char ':'
        , τDoc
        ]
    LocalDef (Variable v) t τ -> do
      tDoc <- prettyDocA t
      τDoc <- prettyDocA τ
      return $ fillSep
        [ text v
        , text ":="
        , tDoc
        , char ':'
        , τDoc
        ]

nameOf :: LocalDeclaration ξ -> Variable
nameOf (LocalAssum v _)   = v
nameOf (LocalDef   v _ _) = v
