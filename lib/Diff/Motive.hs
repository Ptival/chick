{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff.Motive
  ( δmkMotiveType
  ) where

import qualified Diff.Atom as DA
import qualified Diff.Inductive as DI
import           Diff.ListFoldLeft
import           Diff.ListFoldRight
import qualified Diff.Pair as DP
import qualified Diff.Term as DT
import           Inductive.Inductive
import           Term.Term

δonInductiveParameter :: α -> Φips α Variable -> DI.Δps α -> DT.Diff α -> DT.Diff α
δonInductiveParameter α = δListFoldLeft δListFold
  where
    δListFold = ΔListFold
      { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
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

δonInductiveIndexInside :: α -> Φiis α Variable -> DI.Δis α -> DT.Diff α -> DT.Diff α
δonInductiveIndexInside α = δListFoldLeft δListFold
  where
    δListFold = ΔListFold
      { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
    onInsert  (v, _) _ b = DT.InsApp α b (DT.Replace (Var Nothing v))
    onKeep           _ b = DT.CpyApp b DT.Same
    onModify       δ _ b =
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

δonInductiveIndexOutside :: α -> Φiis α Variable -> DI.Δis α -> DT.Diff α -> DT.Diff α
δonInductiveIndexOutside α = δListFoldRight δListFold
  where
    δListFold = ΔListFold
      { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }
    onInsert  (v, i) _ δ = DT.InsPi α (DT.Replace i) (Binder (Just v)) δ
    onKeep           _ δ = DT.CpyPi DT.Same DA.Same δ
    onModify  δe     _ δ = δ'
      where
        δ' = case δe of
          DP.Same         -> DT.CpyPi DT.Same DA.Same δ
          DP.Modify δv δi ->
            let δb = case δv of
                  DA.Replace v -> DA.Replace (Binder (Just v))
                  DA.Same      -> DA.Same
            in
            DT.CpyPi δi δb δ
    onPermute _p _δ = error "TODO: δonInductiveIndexInside"
    onRemove  _b    = error "TODO: δonInductiveIndexInside"
    onReplace _l _δ = error "TODO: δonInductiveIndexInside"
    onSame     l  δ = DT.nCpyPis (length l) δ

δmkMotiveType'
  :: α -> DA.Diff Variable ->
  Φips α Variable -> DI.Δps α ->
  Φiis α Variable -> DI.Δis α ->
  DT.Diff α
δmkMotiveType' α δn ips δips iis δiis =
  δonInductiveIndexOutside  α iis δiis
  $ (\ b -> DT.CpyPi b DA.Same DT.Same)
  $ δonInductiveIndexInside α iis δiis
  $ δonInductiveParameter   α ips δips
  $ DT.CpyVar δn

δmkMotiveType :: α -> Inductive α Variable -> DI.Diff α -> DT.Diff α
δmkMotiveType α (Inductive _ ips iis _) δi = case δi of
  DI.Same -> DT.Same
  DI.Modify δn δips δiis _ -> δmkMotiveType' α δn ips δips iis δiis
