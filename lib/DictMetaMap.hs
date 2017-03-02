{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

module DictMetaMap where

import Term.Term

data DictMetaMap fξ fψ ξ ψ = MkDictMetaMap
  { doAnnot :: fξ (X_Annot ξ) -> fψ (X_Annot ψ)
  , doApp   :: fξ (X_App   ξ) -> fψ (X_App   ψ)
  , doHole  :: fξ (X_Hole  ξ) -> fψ (X_Hole  ψ)
  , doLam   :: fξ (X_Lam   ξ) -> fψ (X_Lam   ψ)
  , doLet   :: fξ (X_Let   ξ) -> fψ (X_Let   ψ)
  , doPi    :: fξ (X_Pi    ξ) -> fψ (X_Pi    ψ)
  , doType  :: fξ (X_Type  ξ) -> fψ (X_Type  ψ)
  , doVar   :: fξ (X_Var   ξ) -> fψ (X_Var   ψ)
  }

dictMetaMap' ::
  (ForallX ((~) a) ξ, ForallX ((~) b) ψ) =>
  (fξ a -> fψ b) -> DictMetaMap fξ fψ ξ ψ
dictMetaMap' f  = MkDictMetaMap f f f f f f f f

metaMap ::
  forall ξ ψ a. DictMetaMap ((,) a) ((,) a) ξ ψ -> a -> TermX ξ -> TermX ψ
metaMap d base = snd . go base
  where
    go :: a -> TermX ξ -> (a, TermX ψ)
    go acc = \case

      Annot a t τ ->
        let (acc1, a') = doAnnot d (acc, a) in
        let (acc2, t') = go acc1 t in
        let (acc3, τ') = go acc2 τ in
        (acc3, Annot a' t' τ')

      App a t1 t2 ->
        let (acc1, a') = doApp d (acc, a) in
        let (acc2, t1') = go acc1 t1 in
        let (acc3, t2') = go acc2 t2 in
        (acc3, App a' t1' t2')

      Hole a ->
        let (acc1, a') = doHole d (acc, a) in
        (acc1, Hole a')

      Lam a n t ->
        let (acc1, a') = doLam d (acc, a) in
        let (acc2, t') = go acc1 t in
        (acc2, Lam a' n t')

      Let a n t1 t2 ->
        let (acc1, a') = doLet d (acc, a) in
        let (acc2, t1') = go acc1 t1 in
        let (acc3, t2') = go acc2 t2 in
        (acc3, Let a' n t1' t2')

      Pi a n τ1 τ2 ->
        let (acc1, a') = doPi d (acc, a) in
        let (acc2, τ1') = go acc1 τ1 in
        let (acc3, τ2') = go acc2 τ2 in
        (acc3, Pi a' n τ1' τ2')

      Type a ->
        let (acc1, a') = doType d (acc, a) in
        (acc1, Type a')

      Var a x ->
        let (acc1, a') = doVar d (acc, a) in
        (acc1, Var a' x)
