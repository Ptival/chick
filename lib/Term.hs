{-# language ConstraintKinds #-}
--{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
--{-# language EmptyCase #-}
{-# language FlexibleContexts #-}
--{-# language FlexibleInstances #-}
--{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
--{-# language OverloadedStrings #-}
--{-# language PatternSynonyms #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
--{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Term where

import Data.Typeable
import GHC.Generics
import GHC.Exts      (Constraint)
import Text.Printf

type Name = String

type family X_Annot ξ
type family X_App   ξ
type family X_Hole  ξ
type family X_Lam   ξ
type family X_Let   ξ
type family X_Pi    ξ
type family X_Type  ξ
type family X_Var   ξ

data TermX ξ
  = Annot (X_Annot ξ) (TermX ξ) (TypeX ξ)
  | App   (X_App   ξ) (TermX ξ) (TermX ξ)
  | Hole  (X_Hole  ξ)
  | Lam   (X_Lam   ξ) (Maybe Name) (TermX ξ)
  | Let   (X_Let   ξ) (Maybe Name) (TermX ξ) (TermX ξ)
  | Pi    (X_Pi    ξ) (Maybe Name) (TypeX ξ) (TermX ξ)
  | Type  (X_Type  ξ)
  | Var   (X_Var   ξ) Name
  deriving (Generic, Typeable)

type TypeX = TermX

type ForallX (φ :: * -> Constraint) ξ =
  ( φ (X_Annot ξ)
  , φ (X_App   ξ)
  , φ (X_Hole  ξ)
  , φ (X_Lam   ξ)
  , φ (X_Let   ξ)
  , φ (X_Pi    ξ)
  , φ (X_Type  ξ)
  , φ (X_Var   ξ)
  )

deriving instance ForallX Show ξ => Show (TermX ξ)

type ForallX2 (φ :: * -> * -> Constraint) ξ ψ =
  ( φ (X_Annot ξ) (X_Annot ψ)
  , φ (X_App   ξ) (X_App   ψ)
  , φ (X_Hole  ξ) (X_Hole  ψ)
  , φ (X_Lam   ξ) (X_Lam   ψ)
  , φ (X_Let   ξ) (X_Let   ψ)
  , φ (X_Pi    ξ) (X_Pi    ψ)
  , φ (X_Type  ξ) (X_Type  ψ)
  , φ (X_Var   ξ) (X_Var   ψ)
  )

-- The arguments are flipped so that I can write:
-- ForallX (ConvertTo TargetType) ξ
-- Since there seems to be no way to do either:
-- ForallX (\ Source -> Convert Source Target) ξ
-- or:
-- type ConvertTo b a = Convert a b
-- The latter works, but you cannot partially-apply it:
-- ForallX (ConvertTo TargetType) ξ
--         ^^^^^^^^^^^^^^^^^^^^^^ Haskell will complain about this
class ConvertTo b a where
  convert :: a -> b

convertMap :: ForallX2 ConvertTo ψ ξ => TermX ξ -> TermX ψ
convertMap = \case
  Annot a t τ     -> Annot (convert a) (convertMap t) (convertMap τ)
  App   a t1 t2   -> App   (convert a) (convertMap t1) (convertMap t2)
  Hole  a         -> Hole  (convert a)
  Lam   a n t     -> Lam   (convert a) n (convertMap t)
  Let   a n t1 t2 -> Let   (convert a) n (convertMap t1) (convertMap t2)
  Pi    a n τ1 τ2 -> Pi    (convert a) n (convertMap τ1) (convertMap τ2)
  Type  a         -> Type  (convert a)
  Var   a x       -> Var   (convert a) x

instance ForallX Show ξ => PrintfArg (TermX ξ) where
  formatArg t = formatString (show t)

annotationOf :: ForallX ((~) a) ξ => TermX ξ -> a
annotationOf = \case
  Annot a _ _   -> a
  App   a _ _   -> a
  Hole  a       -> a
  Lam   a _ _   -> a
  Let   a _ _ _ -> a
  Pi    a _ _ _ -> a
  Type  a       -> a
  Var   a _     -> a
