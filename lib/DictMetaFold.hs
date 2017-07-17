{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

module DictMetaMap where

import Term.Term

data DictMetaFold acc ξ = DictMetaFold
  { doAnnot :: acc -> X_Annot ξ -> acc
  , doApp   :: acc -> X_App   ξ -> acc
  , doHole  :: acc -> X_Hole  ξ -> acc
  , doLam   :: acc -> X_Lam   ξ -> acc
  , doLet   :: acc -> X_Let   ξ -> acc
  , doPi    :: acc -> X_Pi    ξ -> acc
  , doType  :: acc -> X_Type  ξ -> acc
  , doVar   :: acc -> X_Var   ξ -> acc
  }

dictMetaFold' ::
  (ForallX ((~) a) ξ) =>
  (acc -> a -> acc) -> DictMetaFold acc ξ
dictMetaFold' f  = DictMetaFold f f f f f f f f

metaFold :: forall ξ acc. DictMetaFold acc ξ -> acc -> TermX ξ -> acc
metaFold d base = go base
  where
    go :: acc -> TermX ξ -> acc
    go acc = \case

      Annot a t τ ->
        let acc1 = doAnnot d acc a in
        let acc2 = go acc1 t in
        let acc3 = go acc2 τ in
        acc3

      App a t1 t2 ->
        let acc1 = doApp d acc a in
        let acc2 = go acc1 t1 in
        let acc3 = go acc2 t2 in
        acc3

      Hole a ->
        let acc1 = doHole d acc a in
        acc1

      Lam a _n t ->
        let acc1 = doLam d acc a in
        let acc2 = go acc1 t in
        acc2

      Let a _n t1 t2 ->
        let acc1 = doLet d acc a in
        let acc2 = go acc1 t1 in
        let acc3 = go acc2 t2 in
        acc3

      Pi a _n τ1 τ2 ->
        let acc1 = doPi d acc a in
        let acc2 = go acc1 τ1 in
        let acc3 = go acc2 τ2 in
        acc3

      Type a ->
        let acc1 = doType d acc a in
        acc1

      Var a _x ->
        let acc1 = doVar d acc a in
        acc1
