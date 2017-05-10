{-# language ConstraintKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Term.Term where

import Bound
import Bound.Name
import Bound.Scope
import Control.Monad
--import Data.Bifunctor
--import Data.Default
import Data.Functor.Classes
--import Data.String
import Data.Typeable
import GHC.Exts                  (Constraint)
import GHC.Generics
--import Prelude.Extras
--import Term.Binder
--import Term.Variable
--import Test.QuickCheck.Arbitrary
--import Test.QuickCheck.Gen
--import Test.SmallCheck.Series
--import Text.PrettyPrint.GenericPretty (Out)
import Text.Printf

import Term.Binder
import Term.Variable

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

type family X_Annot ξ
type family X_App   ξ
type family X_Hole  ξ
type family X_Lam   ξ
type family X_Let   ξ
type family X_Pi    ξ
type family X_Type  ξ
type family X_Var   ξ

type NameScope = Scope (Name Variable ())

data TermX ξ ν
  = Annot (X_Annot ξ) (TermX ξ ν) (TypeX ξ ν)
  | App   (X_App   ξ) (TermX ξ ν) (TermX ξ ν)
  | Hole  (X_Hole  ξ)
  | Lam   (X_Lam   ξ)             (NameScope (TermX ξ) ν)
  | Let   (X_Let   ξ) (TermX ξ ν) (NameScope (TermX ξ) ν)
  | Pi    (X_Pi    ξ) (TypeX ξ ν) (NameScope (TypeX ξ) ν)
  | Type  (X_Type  ξ)
  | Var   ν
  deriving
    (Functor
    , Generic
    , Typeable
    )

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

instance (Eq ν, Eq1 (TermX ξ)) => Eq (TermX ξ ν) where
  Annot _ t τ == Annot _ t' τ'= t == t' && τ == τ'
  App _ t1 t2 == App _ t1' t2' = t1 == t1' && t2 == t2'
  Hole _ == Hole _ = True
  Lam _ bt == Lam _ bt' = bt == bt'
  Let _ t1 bt2 == Let _ t1' bt2' = t1 == t1' && bt2 == bt2'
  Pi _ τ1 bτ2 == Pi _ τ1' bτ2' = τ1 == τ1' && bτ2 == bτ2'
  Type _ == Type _ = True
  Var v == Var v' = v == v'
  _ == _ = False

instance Eq1 (TermX ξ) where
  liftEq eqVar term1 term2 =
    let (===) = liftEq eqVar in
    case (term1, term2) of
      (Annot _ t τ,  Annot _ t' τ')  -> t === t' && τ === τ'
      (App _ t1 t2,  App _ t1' t2')  -> t1 === t1' && t2 === t2'
      (Hole _,       Hole _)         -> True
      (Lam _ bt,     Lam _ bt')      -> liftEq eqVar bt bt'
      (Let _ t1 bt2, Let _ t1' bt2') -> t1 === t1' && liftEq eqVar bt2 bt2'
      (Pi _ τ1 bτ2,  Pi _ τ1' bτ2')  -> τ1 === τ1' && liftEq eqVar bτ2 bτ2'
      (Type _,       Type _)         -> True
      (Var v,        Var v')         -> eqVar v v'
      (_,            _)              -> False

--deriving instance (ForallX (Serial m) ξ, Monad m) => Serial m  (TermX ξ ν)
--deriving instance  ForallX Out        ξ           => Out       (TermX ξ ν)

instance Applicative (TermX ξ) where
  pure = Var
  (<*>) = ap

instance Monad (TermX ξ) where
  return = Var
  Annot a t  τ   >>= f = Annot a (t   >>= f) (τ  >>= f)
  App   a t1 t2  >>= f = App   a (t1  >>= f) (t2 >>= f)
  Hole  a        >>= _ = Hole  a
  Lam   a bt     >>= f = Lam   a             (bt  >>>= f)
  Let   a t1 bt2 >>= f = Let   a (t1  >>= f) (bt2 >>>= f)
  Pi    a τ1 bτ2 >>= f = Pi    a (τ1  >>= f) (bτ2 >>>= f)
  Type  a        >>= _ = Type  a
  Var     v      >>= f = f v

deriving instance Foldable (TermX ξ)
deriving instance Traversable (TermX ξ)

{-
genTerm ::
  (Arbitrary ν, Default (X_Var ξ), ForallX Arbitrary ξ, CoArbitrary ν) =>
  Int -> Gen (TermX ξ ν)
genTerm 0 =
  frequency
  [ (1, Hole <$> arbitrary)
  , (1, Type <$> arbitrary)
  , (3, Var  <$> arbitrary)
  ]
genTerm n =
  let arbitrary' = choose (0, n-1) >>= genTerm in
  frequency
  [ (1, Annot <$> arbitrary <*> arbitrary' <*> arbitrary')
  , (3, App   <$> arbitrary <*> arbitrary' <*> arbitrary')
  --, Hole  <$> arbitrary
  , (3, Lam   <$> arbitrary <*> arbitrary)
  , (1, Let   <$> arbitrary <*> arbitrary <*> arbitrary)
  , (3, Pi    <$> arbitrary <*> arbitrary <*> arbitrary)
  --, Type  <$> arbitrary
  , (1, Var   <$> arbitrary)
  ]

instance (ForallX Arbitrary ξ, Arbitrary ν, CoArbitrary ν, Default (X_Var ξ)) =>
         Arbitrary (TermX ξ ν) where

  arbitrary = sized genTerm

  shrink = \case

    Annot a t τ ->
      [t, τ] ++ [Annot a' t' τ' | (a', t', τ') <- shrink (a, t, τ)]

    App a t1 t2 ->
      [t1, t2] ++ [App a' t1' t2' | (a', t1', t2') <- shrink (a, t1, t2)]

    Hole _ -> []

    Lam a bt ->
      -- [instantiate1 (Var _ "x") bt]
      -- ++
      [Lam a' bt' | (a', bt') <- shrink (a, bt)]

    Let a t1 bt2 ->
      -- [t1, unscope bt2]
      -- ++
      [Let a' t1' bt2' | (a', t1', bt2') <- shrink (a, t1, bt2)]

    Pi a τ bt ->
      -- [τ, unscope bt]
      -- ++
      [Pi a' τ' bt' | (a', τ', bt') <- shrink (a, τ, bt)]

    Type _ -> []

    Var v -> [Var v' | v' <- shrink v]

-}

instance ForallX Show ξ => Show1 (TermX ξ) where
  liftShowsPrec = error "TODO"
deriving instance (ForallX Show ξ, Show ν) => Show (TermX ξ ν)

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

instance (ForallX Show ξ, Show ν) => PrintfArg (TermX ξ ν) where
  formatArg t = formatString (show t)

{-
We can retrieve the annotation for a term generically only when they all
share the same annotation type. Otherwise, the output type would depend on
the constructor.
-}
annotationOf :: ForallX ((~) a) ξ => TermX ξ ν -> a
annotationOf = \case
  Annot a _ _ -> a
  App   a _ _ -> a
  Hole  a     -> a
  Lam   a _   -> a
  Let   a _ _ -> a
  Pi    a _ _ -> a
  Type  a     -> a
  Var     _   -> error "TODO: I removed annotatoins from Var"

simultaneousSubstitute ::
  (Monad f, Eq a) => [(a, f a)] -> f a -> f a
simultaneousSubstitute l w =
  w >>= \b -> case lookup b l of
                Just p -> p
                Nothing -> return b

abstractAnonymous :: (Monad f) => f ν -> Scope (Name ν ()) f ν
abstractAnonymous = abstractName (const Nothing)

abstractBinder :: (Monad f) => Eq ν => Binder ν -> f ν -> Scope (Name ν ()) f ν
abstractBinder b =
  case unBinder b of
    Nothing -> abstractAnonymous
    Just v  -> abstract1Name v
