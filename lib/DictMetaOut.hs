{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

module DictMetaOut where

import Term.Term

data DictMetaOut o ξ = DictMetaOut
  { onAnnot :: X_Annot ξ -> o
  , onApp   :: X_App   ξ -> o
  , onHole  :: X_Hole  ξ -> o
  , onLam   :: X_Lam   ξ -> o
  , onLet   :: X_Let   ξ -> o
  , onPi    :: X_Pi    ξ -> o
  , onType  :: X_Type  ξ -> o
  , onVar   :: X_Var   ξ -> o
  }

dictMetaOut' :: (ForallX ((~) a) ξ) => (a -> o) -> DictMetaOut o ξ
dictMetaOut' f  = DictMetaOut f f f f f f f f

metaOut :: forall o ξ ν . DictMetaOut o ξ -> TermX ξ ν -> o
metaOut d = go
  where
    go = \case
      Annot a _ _ -> onAnnot d a
      App   a _ _ -> onApp   d a
      Hole  a     -> onHole  d a
      Lam   a _   -> onLam   d a
      Let   a _ _ -> onLet   d a
      Pi    a _ _ -> onPi    d a
      Type  a     -> onType  d a
      Var     _   -> error "TODO: DictMetaOut Var" -- onVar   d a
