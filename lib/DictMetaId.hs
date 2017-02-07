{-# language LambdaCase #-}
{-# language TypeFamilies #-}

module DictMetaId where

import Data.Functor.Identity

import DictMeta
import Term

type DictMetaId = DictMeta Identity

dictMetaId :: ForallX ((~) t) ξ => t -> DictMetaId ξ
dictMetaId = dictMeta' . pure

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

metaId :: DictMetaId ψ -> TermX ξ -> TermX ψ
metaId d = go
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

metaId' :: (ForallX ((~) b) ψ) => b -> TermX ξ -> TermX ψ
metaId' b t = metaId (dictMetaId b) t

metaIdHead :: DictMetaId ξ -> TermX ξ -> TermX ξ
metaIdHead d = go
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

metaIdHead' :: (ForallX ((~) b) ξ) => b -> TermX ξ -> TermX ξ
metaIdHead' b t = metaIdHead (dictMetaId b) t
