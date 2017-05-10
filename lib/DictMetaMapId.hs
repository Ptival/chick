{-# language LambdaCase #-}
{-# language TypeFamilies #-}

module DictMetaMapId where

import Data.Functor.Identity

import DictMetaMap
import Term.Term

type DictMetaMapId = DictMetaMap Identity Identity

metaMapId :: DictMetaMapId ξ ψ -> TermX ξ ν -> TermX ψ ν
metaMapId d = go
  where
    go = \case
      Annot a t τ     -> Annot (doAnnotId d a) (go t) (go τ)
      App   a t1 t2   -> App   (doAppId   d a) (go t1) (go t2)
      Hole  a         -> Hole  (doHoleId  d a)
      Lam   a n t     -> Lam   (doLamId   d a) n (go t)
      Let   a n t1 t2 -> Let   (doLetId   d a) n (go t1) (go t2)
      Pi    a n τ1 τ2 -> Pi    (doPiId    d a) n (go τ1) (go τ2)
      Type  a         -> Type  (doTypeId  d a)
      Var   a x       -> Var   (doVarId   d a) x

{-
dictMetaMapId ::
  (ForallX ((~) a) ξ, ForallX ((~) b) ψ) =>
  (a -> b) -> DictMetaMapId ξ ψ
dictMetaMapId  = dictMetaMap' . fmap

metaMapId' ::
  (ForallX ((~) a) ξ, ForallX ((~) b) ψ) =>
  (a -> b) -> TermX ξ -> TermX ψ
metaMapId' f t = metaMapId (dictMetaMapId f) t

reannotateUniform :: ForallX ((~) b) ψ => b -> TermX ξ -> TermX ψ
reannotateUniform v = metaMapId (dictMetaMapIgnoreLeft (pure v))

doAnnotId :: DictMetaMapId ξ ψ -> X_Annot ξ -> X_Annot ψ
doAnnotId d a = runIdentity (doAnnot d (Identity a))
doAppId   :: DictMetaMapId ξ ψ -> X_App   ξ -> X_App   ψ
doAppId   d a = runIdentity (doApp   d (Identity a))
doHoleId  :: DictMetaMapId ξ ψ -> X_Hole  ξ -> X_Hole  ψ
doHoleId  d a = runIdentity (doHole  d (Identity a))
doLamId   :: DictMetaMapId ξ ψ -> X_Lam   ξ -> X_Lam   ψ
doLamId   d a = runIdentity (doLam   d (Identity a))
doLetId   :: DictMetaMapId ξ ψ -> X_Let   ξ -> X_Let   ψ
doLetId   d a = runIdentity (doLet   d (Identity a))
doPiId    :: DictMetaMapId ξ ψ -> X_Pi    ξ -> X_Pi    ψ
doPiId    d a = runIdentity (doPi    d (Identity a))
doTypeId  :: DictMetaMapId ξ ψ -> X_Type  ξ -> X_Type  ψ
doTypeId  d a = runIdentity (doType  d (Identity a))
doVarId   :: DictMetaMapId ξ ψ -> X_Var   ξ -> X_Var   ψ
doVarId   d a = runIdentity (doVar   d (Identity a))
-}
