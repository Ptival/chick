{-# language ConstraintKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Term where

import Data.List
import Data.Typeable
import GHC.Exts                       (Constraint)
import GHC.Generics
import Text.PrettyPrint.GenericPretty (Out)
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

{- This is NOT a functor -}
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

deriving instance ForallX Out ξ => Out (TermX ξ)

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

instance Show (TermX ξ) where
  show (Annot _ t τ) = printf "%s @ %s" (show t) (show τ)
  show (App   _ t1 t2) = printf "%s %s" (show t1) (show t2)
  show (Hole  _) = "_"
  show t@(Lam   _ _ _) = showLams [] t
  show (Let   _ Nothing  t1 t2) =
    printf "let _ = %s in %s" (show t1) (show t2)
  show (Let   _ (Just n) t1 t2) =
    printf "let %s = %s in %s" n (show t1) (show t2)
  show (Pi    _ Nothing  τ t) = printf "%s → %s" (show τ) (show t)
  show (Pi    _ (Just n) τ t) =
    printf "(%s : %s) → %s" n (show τ) (show t)
  show (Type  _) = "Type"
  show (Var   _ n) = n

showLams :: [String] -> TermX ξ -> String
showLams l (Lam _ Nothing  t) = showLams ("_" : l) t
showLams l (Lam _ (Just n) t) = showLams (n   : l) t
showLams l t =
  printf "λ %s . %s" (concat . intersperse " " . reverse $ l) (show t)

--deriving instance ForallX Show ξ => Show (TermX ξ)

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

instance ForallX Show ξ => PrintfArg (TermX ξ) where
  formatArg t = formatString (show t)

{-
We can retrieve the annotation for a term generically only when they all
share the same annotation type. Otherwise, the output type would depend on
the constructor.
-}
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
