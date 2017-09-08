module Diff.Motive where

import qualified Diff.Atom as DA
import qualified Diff.Inductive as DI
import qualified Diff.Pair as DP
import qualified Diff.Term as DT
import           Diff.ListFoldLeft
import           Term.Term

δonInductiveIndexInside :: α -> DI.Δis' α -> DT.Diff α -> DT.Diff α
δonInductiveIndexInside α =
  δListFoldLeft
  (ΔListFoldLeft
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
   , onSame    = \ _b -> error "TODO: δonInductiveIndexInside"
   })
