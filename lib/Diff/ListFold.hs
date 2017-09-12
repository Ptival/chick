{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE NamedFieldPuns #-}

module Diff.ListFold
  ( ΔListFold(..)
  , δListFoldMkAppTerms
  , δListFoldMkAppVariables
  , δListFoldMkPiBinders
  , δListFoldMkPiGeneric
  , δListFoldMkPiVariables
  )where

import qualified Diff.Atom as DA
import qualified Diff.Binder as DB
import qualified Diff.Pair as DP
import qualified Diff.Term as DT
import           Term.Term

data ΔListFold τ δτ a = ΔListFold
  { onInsert  ::     τ ->      [τ] -> a -> a
  , onKeep    ::          τ -> [τ] -> a -> a
  , onModify  ::    δτ -> τ -> [τ] -> a -> a
  , onPermute :: [Int] ->      [τ] -> a -> a
  , onRemove  ::          τ -> [τ] -> a -> a
  , onReplace ::   [τ] ->      [τ] -> a -> a
  , onSame    ::               [τ] -> a -> a
  }

δListFoldMkAppTerms :: α -> ΔListFold (TermX α Variable) (DT.Diff α) (DT.Diff α)
δListFoldMkAppTerms α = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    onInsert      t   _  b = DT.InsApp α b (DT.Replace t)
    onKeep          _ _  b = DT.CpyApp b DT.Same
    onModify     δt _ _  b = DT.CpyApp b δt
    onPermute     _   _  _ = error "TODO: δonInductiveIndexInside"
    onRemove        _ _  _ = error "TODO: δonInductiveIndexInside"
    onReplace     _   _  _ = error "TODO: δonInductiveIndexInside"
    onSame            l  b = DT.nCpyApps (length l) b

δListFoldMkAppVariables ::
  α ->
  ΔListFold
  (Variable, TermX α Variable)
  (DP.Diff (DA.Diff Variable) (DT.Diff α)) (DT.Diff α)
δListFoldMkAppVariables α = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    onInsert (v, _)   _ b = DT.InsApp α b (DT.Replace (Var Nothing v))
    onKeep          _ _ b = DT.CpyApp b DT.Same
    onModify      δ _ _ b =
      case δ of
      DP.Same -> DT.CpyApp b DT.Same
      DP.Modify δl _ ->
        case δl of
        DA.Same -> DT.CpyApp b DT.Same
        DA.Replace r -> DT.CpyApp b (DT.Replace (Var Nothing r))
    onPermute _ _ _ = error "TODO: δonInductiveIndexInside"
    onRemove  _ _ _ = error "TODO: δonInductiveIndexInside"
    onReplace _ _ _ = error "TODO: δonInductiveIndexInside"
    onSame      l b = DT.nCpyApps (length l) b

δListFoldMkPiGeneric ::
  α ->
  (τ -> (DT.Diff α, Binder Variable)) ->
  (τ -> δτ -> (DT.Diff α, DA.Diff (Binder Variable))) ->
  ΔListFold τ δτ (DT.Diff α)
δListFoldMkPiGeneric α pi δpi = ΔListFold
  { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
  where
    onInsert   e _   δ = DT.InsPi α δτ b δ            where (δτ,  b) =  pi  e
    onKeep       _ _ δ = DT.CpyPi   DT.Same DA.Same δ
    onModify  δe e _ δ = DT.CpyPi   δτ      δb      δ where (δτ, δb) = δpi e δe
    onPermute _p _δ = error "TODO: δonInductiveIndexInside"
    onRemove  _b    = error "TODO: δonInductiveIndexInside"
    onReplace _l _δ = error "TODO: δonInductiveIndexInside"
    onSame     l  δ = DT.nCpyPis (length l) δ

δListFoldMkPiBinders ::
  α ->
  ΔListFold
  (Binder Variable, TermX α Variable)
  (DP.Diff (DA.Diff (Binder Variable)) (DT.Diff α)) (DT.Diff α)
δListFoldMkPiBinders α = δListFoldMkPiGeneric α pi δpi
  where
    pi  (b, i) = (DT.Replace i, b)
    δpi _ δe = case δe of
      DP.Same         -> (DT.Same, DA.Same)
      DP.Modify δb δi -> (δi, δb)

δListFoldMkPiVariables ::
  α ->
  ΔListFold
  (Variable, TermX α Variable)
  (DP.Diff (DA.Diff Variable) (DT.Diff α)) (DT.Diff α)
δListFoldMkPiVariables α = δListFoldMkPiGeneric α pi δpi
  where
    pi  (v, i) = (DT.Replace i, Binder (Just v))
    δpi _ δe = case δe of
      DP.Same         -> (DT.Same, DA.Same)
      DP.Modify δv δi -> (δi, DB.fromΔVariable δv)
