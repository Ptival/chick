module Diff.Guess.Vernacular
  ( guess,
  )
where

import qualified Definition as D
import qualified Diff.Guess.Atom as ΔGA
import qualified Diff.Guess.Inductive as ΔGI
import qualified Diff.Guess.Term as ΔGT
import qualified Diff.Vernacular as ΔV
import Polysemy (Member, Sem)
import Polysemy.Trace (Trace)
import qualified Term.Raw as Raw
import Term.Variable (Variable)
import Vernacular (Vernacular (Definition, Inductive))

guess ::
  Member Trace r =>
  Vernacular Raw.Raw Variable ->
  Vernacular Raw.Raw Variable ->
  Sem r (ΔV.Diff Raw.Raw)
guess (Definition d1) (Definition d2) = do
  δk <- ΔGA.guess (D.definitionKind d1) (D.definitionKind d2)
  δn <- ΔGA.guess (D.definitionName d1) (D.definitionName d2)
  δτ <- ΔGT.guess (D.definitionType d1) (D.definitionType d2)
  δt <- ΔGT.guess (D.definitionTerm d1) (D.definitionTerm d2)
  return $ ΔV.ModifyDefinition δk δn δτ δt
guess (Inductive i1) (Inductive i2) = ΔV.ModifyInductive <$> ΔGI.guess i1 i2
guess _ v2 = return $ ΔV.Replace v2
