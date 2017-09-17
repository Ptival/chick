{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Diff.Eliminator
  ( δeliminatorName
  , δmkEliminatorType
  , δmkEliminatorType'
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Exception
import Control.Monad.Freer.Trace

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
-- import           PrettyPrinting.PrettyPrintable
import           Term.Term
import           Utils

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

δapplyVariables ::
  α ->
  [(Variable, TermX α Variable)] ->
  DL.Diff (Variable, TermX α Variable) (DP.Diff (DA.Diff Variable) (DT.Diff α)) ->
  DT.Diff α -> DT.Diff α
δapplyVariables α = δListFoldLeft (δListFoldMkAppVariables α)

-- This one is different from the regular `δconcatMap`, because the function
-- being `concatMap`-ped also changes.

δListFoldConcatMapAddRecursiveMotive ::
  ( Member (Exc String) r
  , Member Trace r
  ) =>
  α -> Variable -> DA.Diff Variable ->
  Φips α Variable -> DI.Δips α ->
  Φiis α Variable -> DI.Δips α ->
  TermX α Variable -> DT.Diff α ->
  Eff r (ΔListFold (Variable, TypeX α Variable)
         (DP.Diff (DA.Diff Variable) (DT.Diff α))
         (Maybe (DL.Diff (Binder Variable, TypeX α Variable)
                 (DP.Diff (DA.Diff (Binder Variable)) (DT.Diff α))
                )))
δListFoldConcatMapAddRecursiveMotive α n δn ips δips iis δiis motive δmotive = do
  n'      <- DA.patch                   n      δn
  ips'    <- DL.patch DI.patchParameter ips    δips
  iis'    <- DL.patch DI.patchIndex     iis    δiis
  motive' <- DT.patch                   motive δmotive

  let addRec  = addRecursiveMotive α n  ips  iis  motive
  let addRec' = addRecursiveMotive α n' ips' iis' motive'

  let onChanged a a' acc =
        case (addRec a, addRec' a') of
        (e : r, e' : r') -> δFirst e e' . δSecond r r' <$> acc
          where
            δFirst e e' | e == e'   = DL.Keep
                        | otherwise =
                          let (b, τ) = e' in
                          DL.Modify (DP.Modify (DA.Replace b) (DT.Replace τ))
            δSecond []  []   = id
            δSecond []  [m'] = DL.Insert m'
            δSecond [_] []   = DL.Remove
            δSecond [_] [(b, τ)] = DL.Modify (DP.Modify (DA.Replace b) (DT.Replace τ))
            δSecond _   _    = error "THIS SHOULD NOT HAPPEN"
        _ -> error "THIS SHOULD NOT HAPPEN"

  let onInsert a _ acc = foldrWith (\ b acc -> DL.Insert b acc) (addRec' a) <$> acc
  let onKeep   a _ acc = onChanged a a acc
  let onModify δa a _ acc =
        case DP.patchMaybe DA.patchMaybe DT.patchMaybe a δa of
        Nothing -> Nothing
        Just a' -> onChanged a a' acc
  let onPermute _ _ _    = error "TODO: δconcatMapAddRecursiveMotive onPermute"
  let onRemove  _ _ _acc = error "TODO: δconcatMapAddRecursiveMotive onRemove"
  let onReplace _ _ _acc = error "TODO: δconcatMapAddRecursiveMotive onReplace"
  let onSame      l acc = DL.nKeeps (length l) <$> acc

  return $ ΔListFold
    { onInsert, onKeep, onModify, onPermute, onRemove, onReplace, onSame }

δconcatMapAddRecursiveMotive ::
  α ->
  Variable -> DA.Diff Variable ->
  Φips α Variable -> DI.Δips α ->
  Φiis α Variable -> DI.Δiis α ->
  Variable -> DA.Diff Variable ->
  Φcps α Variable -> DC.Δcps α ->
  Φcis α Variable -> DC.Δcis α ->
  TermX α Variable -> DT.Diff α ->
  DL.Diff
  (Binder Variable, TypeX α Variable)
  (DP.Diff (DA.Diff (Binder Variable)) (DT.Diff α)) ->
  Maybe
  (DL.Diff
   (Binder Variable, TypeX α Variable)
   (DP.Diff (DA.Diff (Binder Variable)) (DT.Diff α)))
δconcatMapAddRecursiveMotive
  α n δn ips δips iis δiis _cn _δcn cps δcps _cis _δcis motive δmotive δ = do
  let eff = δListFoldConcatMapAddRecursiveMotive
            α n δn ips δips iis δiis motive δmotive
  case skipTrace $ runError eff of
    Left (_ :: String) -> Nothing
    Right δListFold -> δListFoldRight δListFold cps δcps (Just δ)

δmkCase ::
  α ->
  Variable -> DA.Diff Variable ->
  Φips α Variable -> DI.Δips α ->
  Φiis α Variable -> DI.Δiis α ->
  Variable -> DA.Diff Variable ->
  Φcps α Variable -> DC.Δcps α ->
  Φcis α Variable -> DC.Δcis α ->
  DT.Diff α ->
  DT.Diff α -> Maybe (DT.Diff α)
δmkCase α n δn ips δips iis δiis cn δcn cps δcps cis δcis δmotive =
  let addRec = addRecursiveMotive α n ips iis motive in
  let  hyps =  concatMap addRec cps in
  case δconcatMapAddRecursiveMotive
       α n δn ips δips iis δiis cn δcn cps δcps cis δcis motive δmotive
       DL.Same
  of
  Nothing -> const Nothing
  Just δhyps ->
    Just <$>
    δquantifyBinders α hyps δhyps
    . δapplyTerms α
    [applyVariables α cps (Var Nothing cn)]
    (DL.Modify (δapplyVariables α cps δcps (DT.CpyVar δcn)) DL.Same)
    . δapplyTerms α cis δcis

-- δListFoldMkPiConstructor ::
  -- α -> Variable ->
  -- Φips α Variable -> Φiis α Variable -> TermX α Variable ->
  -- ΔListFold
  -- (Variable, [(Variable, TypeX α Variable)], [TypeX α Variable])
  -- (DP.Diff (DA.Diff Variable) (DT.Diff α)) (DT.Diff α)
δListFoldMkPiConstructor ::
  α -> Variable -> DA.Diff Variable ->
  Φips α Variable -> DI.Δips α ->
  Φiis α Variable -> DI.Δiis α ->
  TermX α Variable ->
  ΔListFold (Constructor α Variable) (DC.Diff α) (Maybe (DT.Diff α))
δListFoldMkPiConstructor α n δn ips δips iis δiis motive =
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
      DC.Modify δcn δcps δcis -> do
        δcase <- δmkCase α n δn ips δips iis δiis cn δcn cps δcps cis δcis DT.Same DT.Same
        return $ (δcase, DA.Same)

δquantifyCases ::
  α -> Variable -> DA.Diff Variable ->
  Φips α Variable -> DI.Δips α ->
  Φiis α Variable -> DI.Δiis α ->
  TermX α Variable ->
  [Constructor α Variable] ->
  DL.Diff (Constructor α Variable) (DC.Diff α) ->
  DT.Diff α -> Maybe (DT.Diff α)
δquantifyCases α n δn ips δips iis δiis motive cs δcs δ =
  δListFoldRight (δListFoldMkPiConstructor α n δn ips δips iis δiis motive) cs δcs (Just δ)

δmkDiscrimineeType ::
  α ->
  DA.Diff Variable ->
  Φiis α Variable -> DI.Δiis α->
  Φips α Variable -> DI.Δips α ->
  DT.Diff α
δmkDiscrimineeType α δn ips δips iis δiis =
  δapplyVariables   α iis δiis
  $ δapplyVariables α ips δips
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
    δquantifyCases α n δn ips δips iis δiis motive cs δcs
    --(unpackConstructors cs) (unpackΔConstructors δcs)
    $ δquantifyVariables α iis δiis
    $ DT.CpyPi δdiscrimineeType DA.Same
    $ DT.CpyApp (δapplyVariables α iis δiis DT.Same)
    $ DT.Same
  )

δmkEliminatorType :: α -> Inductive α Variable -> DI.Diff α -> Maybe (DT.Diff α)
δmkEliminatorType α (Inductive n ips iis cs) δi = case δi of
  DI.Same -> Just DT.Same
  DI.Modify δn δips δiis δcs ->
    δmkEliminatorType' α n δn ips δips iis δiis cs δcs

δeliminatorName :: DI.Diff α -> DA.Diff Variable
δeliminatorName = \case
  DI.Same -> DA.Same
  DI.Modify δn _ _ _ -> case δn of
    DA.Same -> DA.Same
    DA.Replace r -> DA.Replace $ mkEliminatorName r
