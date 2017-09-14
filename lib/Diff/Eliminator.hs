{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff.Eliminator
  ( δmkEliminatorType
  , δmkEliminatorType'
  ) where

-- import Control.Monad.Freer
-- import Control.Monad.Freer.Exception

import qualified Diff.Atom as DA
import           Diff.ConcatMap
import qualified Diff.Constructor as DC
import qualified Diff.Inductive as DI
import qualified Diff.List as DL
import           Diff.ListFoldLeft
import           Diff.ListFoldRight
import           Diff.Motive
import qualified Diff.Pair as DP
import qualified Diff.Term as DT
import           Inductive.Eliminator
import           Inductive.Inductive
import           Term.Term

δquantifyVariables ::
  α ->
  [(Variable, TermX α Variable)] ->
  DL.Diff (Variable, TermX α Variable) (DP.Diff (DA.Diff Variable) (DT.Diff α)) ->
  DT.Diff α -> DT.Diff α
δquantifyVariables α = δListFoldRight (δListFoldMkPiVariables α)

δquantifyBinders ::
  α ->
  [(Binder Variable, TermX α Variable)] ->
  DL.Diff (Binder Variable, TermX α Variable)
  (DP.Diff (DA.Diff (Binder Variable)) (DT.Diff α)) ->
  DT.Diff α -> DT.Diff α
δquantifyBinders α = δListFoldRight (δListFoldMkPiBinders α)

δapplyTerms ::
  α ->
  [TermX α Variable] ->
  DL.Diff (TermX α Variable) (DT.Diff α) ->
  DT.Diff α -> DT.Diff α
δapplyTerms α = δListFoldLeft (δListFoldMkAppTerms α)

δapplyVars ::
  α ->
  [(Variable, TermX α Variable)] ->
  DL.Diff (Variable, TermX α Variable) (DP.Diff (DA.Diff Variable) (DT.Diff α)) ->
  DT.Diff α -> DT.Diff α
δapplyVars α = δListFoldLeft (δListFoldMkAppVariables α)

δmkCase ::
  α ->
  Variable -> Φips α Variable -> Φiis α Variable ->
  Variable ->
  Φcps α Variable -> DC.Δcps α ->
  Φcis α Variable -> DC.Δcis α ->
  DT.Diff α -> Maybe (DT.Diff α)
δmkCase α n ips iis cn cps δcps cis δcis =
  let addRec = addRecursiveMotive α n ips iis motive in
  let  hyps =  concatMap addRec cps in
  case δconcatMap addRec
       (\ δcp cp -> DP.patchMaybe DA.patchMaybe DT.patchMaybe cp δcp)
       cps δcps of
  Nothing -> const Nothing
  Just δhyps -> Just <$>
    δquantifyBinders α hyps δhyps
    --δquantifyVariables α cps δcps
    -- TODO: recursive motive
    . δapplyTerms α [applyVariables α cps (Var Nothing cn)] DL.Same
    . δapplyTerms α cis δcis

-- δListFoldMkPiConstructor ::
  -- α -> Variable ->
  -- Φips α Variable -> Φiis α Variable -> TermX α Variable ->
  -- ΔListFold
  -- (Variable, [(Variable, TypeX α Variable)], [TypeX α Variable])
  -- (DP.Diff (DA.Diff Variable) (DT.Diff α)) (DT.Diff α)
δListFoldMkPiConstructor ::
  α -> Variable ->
  Φips α Variable -> Φiis α Variable ->
  TermX α Variable ->
  ΔListFold (Constructor α Variable) (DC.Diff α) (Maybe (DT.Diff α))
δListFoldMkPiConstructor α n ips iis motive =
  δListFoldMkPiGenericMaybe α pi δpi
  where
    pi (Constructor _ consName consParameters consIndices) =
      ( DT.Replace
        (mkCase α n ips iis
         consName consParameters consIndices motive)
      , Binder Nothing
      )
    δpi (Constructor _ cn cps cis) = \case
      DC.Same                 -> Just (DT.Same, DA.Same)
      DC.Modify _ δcps δcis -> do
        δcase <- δmkCase α n ips iis cn cps δcps cis δcis DT.Same
        return (δcase, DA.Same)

δquantifyCases ::
  α -> Variable ->
  Φips α Variable ->
  Φiis α Variable ->
  TermX α Variable ->
  [Constructor α Variable] ->
  DL.Diff (Constructor α Variable) (DC.Diff α) ->
  DT.Diff α -> Maybe (DT.Diff α)
δquantifyCases α n ips iis motive cs δcs δ =
  δListFoldRight (δListFoldMkPiConstructor α n ips iis motive) cs δcs (Just δ)

δmkDiscrimineeType ::
  α ->
  DA.Diff Variable ->
  Φiis α Variable -> DI.Δiis α->
  Φips α Variable -> DI.Δips α ->
  DT.Diff α
δmkDiscrimineeType α δn ips δips iis δiis =
  δapplyVars   α iis δiis
  $ δapplyVars α ips δips
  $ DT.CpyVar δn

δmkEliminatorType' ::
  α ->
  Variable -> DA.Diff Variable ->
  Φips α Variable -> DI.Δips α ->
  Φiis α Variable -> DI.Δiis α ->
  [Constructor α Variable] -> DI.Δcs α ->
  Maybe (DT.Diff α)
δmkEliminatorType' α n δn ips δips iis δiis cs δcs =
  let δdiscrimineeType = δmkDiscrimineeType α δn ips δips iis δiis in
  let δmotiveType =  δmkMotiveType' α δn ips δips iis δiis in
  δquantifyVariables α ips δips
  <$> DT.CpyPi δmotiveType DA.Same
  <$> ( -- quantifyCases may fail
    δquantifyCases α n ips iis motive cs δcs
    --(unpackConstructors cs) (unpackΔConstructors δcs)
    $ δquantifyVariables α iis δiis
    $ DT.CpyPi δdiscrimineeType DA.Same
    $ DT.CpyApp (δapplyVars α iis δiis DT.Same)
    $ DT.Same
  )

δmkEliminatorType :: α -> Inductive α Variable -> DI.Diff α -> Maybe (DT.Diff α)
δmkEliminatorType α (Inductive n ips iis cs) δi = case δi of
  DI.Same -> Just DT.Same
  DI.Modify δn δips δiis δcs ->
    δmkEliminatorType' α n δn ips δips iis δiis cs δcs
