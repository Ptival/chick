{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}

module DictMeta where

import Term.Term

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

dictMeta' :: ForallX ((~) t) ξ => φ t -> DictMeta φ ξ
dictMeta' t = MkDictMeta t t t t t t t t
