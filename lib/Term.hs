{-# language ConstraintKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language PatternSynonyms #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
--{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Term where

import Control.Lens
import Data.Typeable
import GHC.Exts      (Constraint)
import GHC.Generics
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

{-
data DictMeta ξ = MkDictMeta
  { metaAnnot :: X_Annot ξ
  , metaApp   :: X_App   ξ
  , metaHole  :: X_Hole  ξ
  , metaLam   :: X_Lam   ξ
  , metaLet   :: X_Let   ξ
  , metaPi    :: X_Pi    ξ
  , metaType  :: X_Type  ξ
  , metaVar   :: X_Var   ξ
  }

dictMeta' :: ForallX ((~) t) ξ => t -> DictMeta ξ
dictMeta' x = MkDictMeta x x x x x x x x
-}

data DictMeta (φ :: * -> *) ξ = MkDictMeta
  { metaAnnot :: φ (X_Annot ξ)
  , metaApp   :: φ (X_App   ξ)
  , metaHole  :: φ (X_Hole  ξ)
  , metaLam   :: φ (X_Lam   ξ)
  , metaLet   :: φ (X_Let   ξ)
  , metaPi    :: φ (X_Pi    ξ)
  , metaType  :: φ (X_Type  ξ)
  , metaVar   :: φ (X_Var   ξ)
  }

type DictMetaId = DictMeta Identity

dictMeta' :: ForallX ((~) t) ξ => φ t -> DictMeta φ ξ
dictMeta' t = MkDictMeta t t t t t t t t

-- as a special case:
dictMetaId :: ForallX ((~) t) ξ => t -> DictMetaId ξ
dictMetaId t = dictMeta' (Identity t)

metaAnnotId :: DictMetaId ξ -> X_Annot ξ
metaAnnotId = runIdentity . metaAnnot

metaAppId :: DictMetaId ξ -> X_App ξ
metaAppId = runIdentity . metaApp

metaHoleId :: DictMetaId ξ -> X_Hole ξ
metaHoleId = runIdentity . metaHole

metaLamId :: DictMetaId ξ -> X_Lam ξ
metaLamId = runIdentity . metaLam

metaLetId :: DictMetaId ξ -> X_Let ξ
metaLetId = runIdentity . metaLet

metaPiId :: DictMetaId ξ -> X_Pi ξ
metaPiId = runIdentity . metaPi

metaTypeId :: DictMetaId ξ -> X_Type ξ
metaTypeId = runIdentity . metaType

metaVarId :: DictMetaId ξ -> X_Var ξ
metaVarId = runIdentity . metaVar

data DictMetaMap ξ ψ = MkDictMetaMap
  { doAnnot :: X_Annot ξ -> X_Annot ψ
  , doApp   :: X_App   ξ -> X_App   ψ
  , doHole  :: X_Hole  ξ -> X_Hole  ψ
  , doLam   :: X_Lam   ξ -> X_Lam   ψ
  , doLet   :: X_Let   ξ -> X_Let   ψ
  , doPi    :: X_Pi    ξ -> X_Pi    ψ
  , doType  :: X_Type  ξ -> X_Type  ψ
  , doVar   :: X_Var   ξ -> X_Var   ψ
  }

dictMetaMap' ::
  ( ForallX ((~) a) ξ
  , ForallX ((~) b) ψ
  ) =>
  (a -> b) -> DictMetaMap ξ ψ
dictMetaMap' f = MkDictMetaMap f f f f f f f f

{- Plug in metadata everywhere -}
meta :: DictMetaId ψ -> TermX ξ -> TermX ψ
meta d = go
  where
    go = \case
      Annot _ t τ     -> Annot (metaAnnotId d) (go t) (go τ)
      App   _ t1 t2   -> App   (metaAppId   d) (go t1) (go t2)
      Hole  _         -> Hole  (metaHoleId  d)
      Lam   _ n t     -> Lam   (metaLamId   d) n (go t)
      Let   _ n t1 t2 -> Let   (metaLetId   d) n (go t1) (go t2)
      Pi    _ n τ1 τ2 -> Pi    (metaPiId    d) n (go τ1) (go τ2)
      Type  _         -> Type  (metaTypeId  d)
      Var   _ x       -> Var   (metaVarId   d) x

meta' :: (ForallX ((~) b) ψ) => b -> TermX ξ -> TermX ψ
meta' b t = meta (dictMetaId b) t

{- Plug in metadata only for the head constructor -}
metaHead :: DictMetaId ξ -> TermX ξ -> TermX ξ
metaHead d = go
  where
    go = \case
      Annot _ t τ     -> Annot (metaAnnotId d) t τ
      App   _ t1 t2   -> App   (metaAppId   d) t1 t2
      Hole  _         -> Hole  (metaHoleId  d)
      Lam   _ n t     -> Lam   (metaLamId   d) n t
      Let   _ n t1 t2 -> Let   (metaLetId   d) n t1 t2
      Pi    _ n τ1 τ2 -> Pi    (metaPiId    d) n τ1 τ2
      Type  _         -> Type  (metaTypeId  d)
      Var   _ x       -> Var   (metaVarId   d) x

metaHead' :: (ForallX ((~) b) ξ) => b -> TermX ξ -> TermX ξ
metaHead' b t = metaHead (dictMetaId b) t

{- Transform metadata -}
metaMap :: DictMetaMap ξ ψ -> TermX ξ -> TermX ψ
metaMap d = go
  where
    go = \case
      Annot a t τ     -> Annot (doAnnot d a) (go t) (go τ)
      App   a t1 t2   -> App   (doApp   d a) (go t1) (go t2)
      Hole  a         -> Hole  (doHole  d a)
      Lam   a n t     -> Lam   (doLam   d a) n (go t)
      Let   a n t1 t2 -> Let   (doLet   d a) n (go t1) (go t2)
      Pi    a n τ1 τ2 -> Pi    (doPi    d a) n (go τ1) (go τ2)
      Type  a         -> Type  (doType  d a)
      Var   a x       -> Var   (doVar   d a) x

metaMap' ::
  (ForallX ((~) a) ξ, ForallX ((~) b) ψ) =>
  (a -> b) -> TermX ξ -> TermX ψ
metaMap' f t = metaMap (dictMetaMap' f) t

data DictMetaFold ξ φξ ψ φψ = MkDictMetaFold
  { foldAnnot :: φξ (X_Annot ξ) -> φψ (X_Annot ψ)
  , foldApp   :: φξ (X_App   ξ) -> φψ (X_App   ψ)
  , foldHole  :: φξ (X_Hole  ξ) -> φψ (X_Hole  ψ)
  , foldLam   :: φξ (X_Lam   ξ) -> φψ (X_Lam   ψ)
  , foldLet   :: φξ (X_Let   ξ) -> φψ (X_Let   ψ)
  , foldPi    :: φξ (X_Pi    ξ) -> φψ (X_Pi    ψ)
  , foldType  :: φξ (X_Type  ξ) -> φψ (X_Type  ψ)
  , foldVar   :: φξ (X_Var   ξ) -> φψ (X_Var   ψ)
  }

dictMetaFold' ::
  ( ForallX ((~) a) ξ
  , ForallX ((~) b) ψ
  ) =>
  (φξ a -> φψ b) -> DictMetaFold ξ φξ ψ φψ
dictMetaFold' f  = MkDictMetaFold f f f f f f f f

{- Not really a fold, more like a stateful map -}
metaFold :: forall ξ ψ a. DictMetaFold ξ ((,) a) ψ ((,) a) -> a -> TermX ξ -> TermX ψ
metaFold d base = snd . go base
  where
    go :: a -> TermX ξ -> (a, TermX ψ)
    go acc = \case

      Annot a t τ ->
        let (acc1, a') = foldAnnot d (acc, a) in
        let (acc2, t') = go acc1 t in
        let (acc3, τ') = go acc2 τ in
        (acc3, Annot a' t' τ')

      App a t1 t2 ->
        let (acc1, a') = foldApp d (acc, a) in
        let (acc2, t1') = go acc1 t1 in
        let (acc3, t2') = go acc2 t2 in
        (acc3, App a' t1' t2')

      Hole a ->
        let (acc1, a') = foldHole d (acc, a) in
        (acc1, Hole a')

      Lam a n t ->
        let (acc1, a') = foldLam d (acc, a) in
        let (acc2, t') = go acc1 t in
        (acc2, Lam a' n t')

      Let a n t1 t2 ->
        let (acc1, a') = foldLet d (acc, a) in
        let (acc2, t1') = go acc1 t1 in
        let (acc3, t2') = go acc2 t2 in
        (acc3, Let a' n t1' t2')

      Pi a n τ1 τ2 ->
        let (acc1, a') = foldPi d (acc, a) in
        let (acc2, τ1') = go acc1 τ1 in
        let (acc3, τ2') = go acc2 τ2 in
        (acc3, Pi a' n τ1' τ2')

      Type a ->
        let (acc1, a') = foldType d (acc, a) in
        (acc1, Type a')

      Var a x ->
        let (acc1, a') = foldVar d (acc, a) in
        (acc1, Var a' x)

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

newtype FlipGetter a b = FlipGetter { unFlipGetter :: Getter   b a }
newtype FlipSetter a b = FlipSetter { unFlipSetter :: ASetter' b a }

lensSetTerm ::
  forall ξ v.
  DictMeta (FlipSetter v) ξ ->
  v -> TermX ξ -> TermX ξ
lensSetTerm ds v =
  metaMap $ MkDictMetaMap
  (modify (metaAnnot ds))
  (modify (metaApp   ds))
  (modify (metaHole  ds))
  (modify (metaLam   ds))
  (modify (metaLet   ds))
  (modify (metaPi    ds))
  (modify (metaType  ds))
  (modify (metaVar   ds))
  where
    modify :: FlipSetter v t -> t -> t
    modify s t = set (unFlipSetter s) v t

lensGetSetTerm ::
  forall ξ i o.
  DictMeta (FlipGetter i) ξ ->
  DictMeta (FlipSetter o) ξ ->
  (i -> o) -> TermX ξ -> TermX ξ
lensGetSetTerm dg ds f =
  metaMap $ MkDictMetaMap
  (modify (metaAnnot dg) (metaAnnot ds))
  (modify (metaApp   dg) (metaApp   ds))
  (modify (metaHole  dg) (metaHole  ds))
  (modify (metaLam   dg) (metaLam   ds))
  (modify (metaLet   dg) (metaLet   ds))
  (modify (metaPi    dg) (metaPi    ds))
  (modify (metaType  dg) (metaType  ds))
  (modify (metaVar   dg) (metaVar   ds))
  where
    modify :: FlipGetter i t -> FlipSetter o t -> t -> t
    modify g s t = set (unFlipSetter s) (f $ t ^. unFlipGetter g) t
