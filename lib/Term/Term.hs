{-# language ConstraintKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Term.Term where

import Data.Typeable
import GHC.Exts                       (Constraint)
import GHC.Generics
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.SmallCheck.Series
import Text.PrettyPrint.GenericPretty (Out)
import Text.Printf

{-
The type-ascription symbol unfortunately cannot be ":" like you would expect,
because in the presence of the four syntactic constructs:

`(t)`
`t → t`
`t : t`
`(t : t) → t`

The string `(t : t) → t` can be decomposed in two non-equivalent ways.

One could either:
- change the Pi syntax, for instance `[t : t] → t`, but people would be surprised by
  the meaning of `(t : t) → t`
- change the Pi syntax, for instance `Π (t : t) → t`, but people would be surprised by
  the meaning of `(t : t) → t`

-}

annotSymbol, holeSymbol :: String
annotSymbol = "@"
holeSymbol  = "?"

type Variable = String

newtype Binder
  = Binder (Maybe Variable)
  deriving (Eq, Generic, Out, Serial m, Show)

arbitraryVariable :: Gen Variable
arbitraryVariable = do
  c <- choose ('a', 'e')
  return [c]
  --take 2 <$> (listOf1 $ oneof [choose ('a', 'z')])

instance Arbitrary Binder where
  arbitrary =
    frequency
    [ (1, return . Binder $ Nothing)
    , (9, Binder . Just <$> arbitraryVariable)
    ]
  shrink (Binder b) = case b of
    Nothing -> []
    Just v  -> [Binder Nothing] ++ [Binder (Just v') | v' <- shrink v]

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
  | Lam   (X_Lam   ξ) Binder (TermX ξ)
  | Let   (X_Let   ξ) Binder (TermX ξ) (TermX ξ)
  | Pi    (X_Pi    ξ) Binder (TypeX ξ) (TermX ξ)
  | Type  (X_Type  ξ)
  | Var   (X_Var   ξ) Variable
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

deriving instance  ForallX Eq         ξ           => Eq        (TermX ξ)
deriving instance (ForallX (Serial m) ξ, Monad m) => Serial m  (TermX ξ)
deriving instance  ForallX Out        ξ           => Out       (TermX ξ)

genTerm :: ForallX Arbitrary ξ => Int -> Gen (TermX ξ)
genTerm 0 =
  frequency
  [ (1, Hole <$> arbitrary)
  , (1, Type <$> arbitrary)
  , (3, Var  <$> arbitrary <*> arbitraryVariable)
  ]
genTerm n =
  let arbitrary' = choose (0, n-1) >>= genTerm in
  oneof
  [ Annot <$> arbitrary <*> arbitrary' <*> arbitrary'
  , App   <$> arbitrary <*> arbitrary' <*> arbitrary'
  --, Hole  <$> arbitrary
  , Lam   <$> arbitrary <*> arbitrary <*> arbitrary'
  , Let   <$> arbitrary <*> arbitrary <*> arbitrary' <*> arbitrary'
  , Pi    <$> arbitrary <*> arbitrary <*> arbitrary' <*> arbitrary'
  --, Type  <$> arbitrary
  , Var   <$> arbitrary <*> arbitraryVariable
  ]

instance ForallX Arbitrary ξ => Arbitrary (TermX ξ) where

  arbitrary = sized genTerm

  shrink = \case

    Annot a t τ ->
      [t, τ] ++ [Annot a' t' τ' | (a', t', τ') <- shrink (a, t, τ)]

    App a t1 t2 ->
      [t1, t2] ++ [App a' t1' t2' | (a', t1', t2') <- shrink (a, t1, t2)]

    Hole _ -> []

    Lam a b t ->
      [t] ++ [Lam a' b' t' | (a', b', t') <- shrink (a, b, t)]

    Let a b t1 t2 ->
      [t1, t2] ++ [Let a' b' t1' t2' | (a', b', t1', t2') <- shrink (a, b, t1, t2)]

    Pi a b τ t ->
      [τ, t] ++ [Pi a' b' τ' t' | (a', b', τ', t') <- shrink (a, b, τ, t)]

    Type _ -> []

    Var a (c : _ : _ ) -> [Var a [c]]
    Var _ _            -> []

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
