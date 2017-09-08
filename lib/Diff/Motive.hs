{-# LANGUAGE NamedFieldPuns #-}

module Diff.Motive where

import qualified Diff.Atom as DA
import qualified Diff.Inductive as DI
import           Diff.ListFoldLeft
import           Diff.ListFoldRight
import qualified Diff.Pair as DP
import qualified Diff.Term as DT
import           Term.Term

δonInductiveParameter :: α -> DI.Δps α -> DT.Diff α -> DT.Diff α
δonInductiveParameter α =
  δListFoldLeft
  (ΔListFold
   { onInsert  = \ (v, _) b -> DT.InsApp α b (DT.Replace (Var Nothing v))
   , onKeep    = \ b -> DT.CpyApp b DT.Same
   , onModify  =
     \ δ b ->
     case δ of
     DP.Same -> DT.CpyApp b DT.Same
     DP.Modify δl _ ->
       case δl of
       DA.Same -> DT.CpyApp b DT.Same
       DA.Replace r -> DT.CpyApp b (DT.Replace (Var Nothing r))
   , onPermute = \ _p _b -> error "TODO: δonInductiveIndexInside"
   , onRemove  = \ _b -> error "TODO: δonInductiveIndexInside"
   , onReplace = \ _l _b -> error "TODO: δonInductiveIndexInside"
   , onSame    = \ b -> b
   })

δonInductiveIndexInside :: α -> DI.Δis α -> DT.Diff α -> DT.Diff α
δonInductiveIndexInside α =
  δListFoldLeft
  (ΔListFold
   { onInsert  = \ (v, _) b -> DT.InsApp α b (DT.Replace (Var Nothing v))
   , onKeep    = \ b -> DT.CpyApp b DT.Same
   , onModify  =
     \ δ b ->
     case δ of
     DP.Same -> DT.CpyApp b DT.Same
     DP.Modify δl _ ->
       case δl of
       DA.Same -> DT.CpyApp b DT.Same
       DA.Replace r -> DT.CpyApp b (DT.Replace (Var Nothing r))
   , onPermute = \ _p _b -> error "TODO: δonInductiveIndexInside"
   , onRemove  = \ _b -> error "TODO: δonInductiveIndexInside"
   , onReplace = \ _l _b -> error "TODO: δonInductiveIndexInside"
   , onSame    = \ b -> b
   })

δonInductiveIndexOutside :: α -> DI.Δis α -> DT.Diff α -> DT.Diff α
δonInductiveIndexOutside α =
  δListFoldRight
  (ΔListFold { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame })
  where
   onInsert  (v, i) δ = DT.InsPi α (DT.Replace i) (Binder (Just v)) δ
   onKeep    δ        = DT.CpyPi DT.Same DA.Same δ
   onModify  δe     δ = case δe of
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
   onSame       δ  = δ

δmkMotiveType'
  :: α -> DI.Δps α -> DI.Δis α -> DT.Diff α
δmkMotiveType' α δps δis =
  δonInductiveIndexOutside  α δis
  $ (\ b -> DT.CpyPi b DA.Same DT.Same)
  $ δonInductiveIndexInside α δis
  $ δonInductiveParameter   α δps
  $ DT.Same
