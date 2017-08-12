{-# LANGUAGE DeriveFoldable #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
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
  | LocalDef
    { localDefName :: ν
    , localDefType :: TypeX ξ ν
    , localDefTerm :: TermX ξ ν
    }

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
  PrettyPrintableUnannotated (TermX α Variable) =>
  PrettyPrintableUnannotated (LocalDeclaration α Variable) where
  prettyDocU = \case
    LocalAssum (Variable v) τ -> do
      τDoc <- prettyDocU τ
      return $ fillSep
        [ text v
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

nameOf :: LocalDeclaration ξ ν -> ν
nameOf (LocalAssum v _)   = v
nameOf (LocalDef   v _ _) = v

typeOf :: LocalDeclaration α ν -> TypeX α ν
typeOf (LocalAssum _ τ  ) = τ
typeOf (LocalDef   _ τ _) = τ
