{-# LANGUAGE DeriveFoldable #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Typing.LocalDeclaration where

--import Test.QuickCheck

import Text.PrettyPrint.Annotated.WL

import PrettyPrinting.PrettyPrintableUnannotated
import Term.Term
import Term.Variable

data LocalDeclaration ξ ν
  = LocalAssum ν (TypeX ξ ν)
  | LocalDef   ν (TermX ξ ν) (TypeX ξ ν)

deriving instance (Eq ξ, Eq ν) => Eq (LocalDeclaration ξ ν)
deriving instance (Show ξ, Show ν) => Show (LocalDeclaration ξ ν)

{-
instance (Arbitrary ξ) => Arbitrary (LocalDeclaration ξ) where
  arbitrary =
    oneof
    [ LocalAssum <$> arbitrary <*> genTerm 2
    , LocalDef   <$> arbitrary <*> genTerm 2 <*> genTerm 2
    ]
-}

instance
  PrettyPrintableUnannotated (TermX ξ) =>
  PrettyPrintableUnannotated (LocalDeclaration ξ) where
  prettyDocU = \case
    LocalAssum (Variable v) τ -> do
      τDoc <- prettyDocU τ
      return $ fillSep
        [ text v
        , char ':'
        , τDoc
        ]
    LocalDef (Variable v) t τ -> do
      tDoc <- prettyDocU t
      τDoc <- prettyDocU τ
      return $ fillSep
        [ text v
        , text ":="
        , tDoc
        , char ':'
        , τDoc
        ]

nameOf :: LocalDeclaration ξ ν -> ν
nameOf (LocalAssum v _)   = v
nameOf (LocalDef   v _ _) = v

typeOf :: LocalDeclaration α ν -> TypeX α ν
typeOf (LocalAssum _ τ) = τ
typeOf (LocalDef _ _ τ) = τ
