{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns #-}

module Diff.ListFold
  ( ΔListFold(..)
  , δListFoldMkApp
  , δListFoldMkPiGeneric
  , δListFoldMkPi
  )where

import qualified Diff.Atom as DA
import qualified Diff.Pair as DP
import qualified Diff.Term as DT
import           Term.Term

data ΔListFold τ δτ a = ΔListFold
  { onInsert  ::     τ -> [τ] -> a -> a
  , onKeep    ::          [τ] -> a -> a
  , onModify  ::    δτ -> [τ] -> a -> a
  , onPermute :: [Int] -> [τ] -> a -> a
  , onRemove  ::          [τ] -> a -> a
  , onReplace ::   [τ] -> [τ] -> a -> a
  , onSame    ::          [τ] -> a -> a
  }

δListFoldMkApp ::
  α ->
  ΔListFold
  (Variable, TermX α Variable)
  (DP.Diff (DA.Diff Variable) (DT.Diff α)) (DT.Diff α)
δListFoldMkApp α = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    onInsert (v, _) _ b = DT.InsApp α b (DT.Replace (Var Nothing v))
    onKeep          _ b = DT.CpyApp b DT.Same
    onModify      δ _ b =
      case δ of
      DP.Same -> DT.CpyApp b DT.Same
      DP.Modify δl _ ->
        case δl of
        DA.Same -> DT.CpyApp b DT.Same
        DA.Replace r -> DT.CpyApp b (DT.Replace (Var Nothing r))
    onPermute _p _ _b = error "TODO: δonInductiveIndexInside"
    onRemove     _ _b = error "TODO: δonInductiveIndexInside"
    onReplace _l _ _b = error "TODO: δonInductiveIndexInside"
    onSame       l  b = DT.nCpyApps (length l) b

δListFoldMkPiGeneric ::
  α ->
  (a -> (DT.Diff α, Binder Variable)) ->
  (t -> (DT.Diff α, DA.Diff (Binder Variable))) ->
  ΔListFold a t (DT.Diff α)
δListFoldMkPiGeneric α pi δpi = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    onInsert  e  _ δ = DT.InsPi α δτ b δ            where (δτ,  b) =  pi  e
    onKeep       _ δ = DT.CpyPi   DT.Same DA.Same δ
    onModify  δe _ δ = DT.CpyPi   δτ      δb      δ where (δτ, δb) = δpi δe
    onPermute _p _δ = error "TODO: δonInductiveIndexInside"
    onRemove  _b    = error "TODO: δonInductiveIndexInside"
    onReplace _l _δ = error "TODO: δonInductiveIndexInside"
    onSame     l  δ = DT.nCpyPis (length l) δ

δListFoldMkPi ::
  α ->
  ΔListFold
  (Variable, TermX α Variable)
  (DP.Diff (DA.Diff Variable) (DT.Diff α)) (DT.Diff α)
δListFoldMkPi α = δListFoldMkPiGeneric α pi δpi
  where
    pi  (v, i) = (DT.Replace i, Binder (Just v))
    δpi δe = case δe of
      DP.Same         -> (DT.Same, DA.Same)
      DP.Modify δv δi ->
        let δb = case δv of
              DA.Replace v -> DA.Replace (Binder (Just v))
              DA.Same      -> DA.Same
        in
        (δi, δb)
