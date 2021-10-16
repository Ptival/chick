{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Diff.Eliminator
  ( δeliminatorName,
    δmkEliminatorType,
    δmkEliminatorType',
  )
where

import Data.Default (Default)
import qualified Diff.Atom as DA
import qualified Diff.Constructor as DC
import qualified Diff.Inductive as DI
import qualified Diff.List as DL
import Diff.ListFold (ΔListFold (..), δListFoldMkPiGenericMaybe)
import Diff.ListFoldRight
  ( δListFoldRight,
  )
import Diff.ListFoldUtils
  ( δapplyBinders,
    δapplyTerms,
    δapplyVariables,
    δquantifyBinders,
    δquantifyVariables,
  )
import Diff.Motive (δmkMotiveType')
import qualified Diff.Pair as D2
import qualified Diff.Term as DT
import qualified Diff.Triple as D3
import Inductive.Eliminator
  ( addRecursiveMotive,
    mkCase,
    mkEliminatorName,
    motive,
  )
import Inductive.Inductive
  ( Constructor (Constructor),
    Inductive (Inductive),
    Φcis,
    Φcps,
    Φiis,
    Φips,
  )
import Inductive.Utils (applyBinders)
import Polysemy (Member, Sem, run)
import Polysemy.Error (Error, runError)
import Polysemy.Trace (Trace, ignoreTrace)
import Term.Term (Binder (Binder), TermX (Var), TypeX, Variable)
import Utils (foldrWith)

-- This one is different from the regular `δconcatMap`, because the function
-- being `concatMap`-ped also changes.

δListFoldConcatMapAddRecursiveMotive ::
  Eq α =>
  Member (Error String) r =>
  Member Trace r =>
  -- , PrettyPrintable α
  Show α =>
  Variable ->
  DA.Diff Variable ->
  Φips α Variable ->
  DI.Δips α ->
  Φiis α Variable ->
  DI.Δiis α ->
  TermX α Variable ->
  DT.Diff α ->
  -- oh gee Rick...
  Sem
    r
    ( ΔListFold
        (α, Binder Variable, TypeX α Variable)
        (D3.Diff (DA.Diff α) (DA.Diff (Binder Variable)) (DT.Diff α))
        ( Maybe
            ( DL.Diff
                (α, Binder Variable, TypeX α Variable)
                (D3.Diff (DA.Diff α) (DA.Diff (Binder Variable)) (DT.Diff α))
            )
        )
    )
δListFoldConcatMapAddRecursiveMotive n δn ips δips iis δiis motive δmotive = do
  n' <- DA.patch n δn
  ips' <- DL.patch DI.patchParameter ips δips
  iis' <- DL.patch DI.patchIndex iis δiis
  motive' <- DT.patch motive δmotive

  let addRec = addRecursiveMotive n ips iis motive
  let addRec' = addRecursiveMotive n' ips' iis' motive'

  let onChanged a a' acc =
        case (addRec a, addRec' a') of
          (e : r, e' : r') -> δFirst e e' . δSecond r r' <$> acc
            where
              δFirst e e'
                | e == e' = DL.Keep
                | otherwise =
                  let (α, b, τ) = e'
                   in DL.Modify (D3.Modify (DA.Replace α) (DA.Replace b) (DT.Replace τ))
              δSecond [] [] = id
              δSecond [] [m'] = DL.Insert m'
              δSecond [_] [] = DL.Remove
              δSecond [_] [(α, b, τ)] =
                DL.Modify (D3.Modify (DA.Replace α) (DA.Replace b) (DT.Replace τ))
              δSecond _ _ = error "THIS SHOULD NOT HAPPEN"
          _ -> error "THIS SHOULD NOT HAPPEN"

  let onInsert a _ acc = foldrWith DL.Insert (addRec' a) <$> acc
  let onKeep a _ = onChanged a a
  let onModify δa a _ acc =
        case D3.patchMaybe DA.patchMaybe DA.patchMaybe DT.patchMaybe a δa of
          Nothing -> Nothing
          Just a' -> onChanged a a' acc
  let onPermute _ _ _ = error "TODO: δconcatMapAddRecursiveMotive onPermute"
  let onRemove removed _ acc =
        let howManyToRemove = length (addRec removed)
         in DL.nRemoves howManyToRemove <$> acc
  let onReplace _ _ _acc = error "TODO: δconcatMapAddRecursiveMotive onReplace"
  let onSame l acc = DL.nKeeps (length l) <$> acc

  return $
    ΔListFold
      { onInsert,
        onKeep,
        onModify,
        onPermute,
        onRemove,
        onReplace,
        onSame
      }

δconcatMapAddRecursiveMotive ::
  Eq α =>
  --, PrettyPrintable α
  Show α =>
  Variable ->
  DA.Diff Variable ->
  Φips α Variable ->
  DI.Δips α ->
  Φiis α Variable ->
  DI.Δiis α ->
  Variable ->
  DA.Diff Variable ->
  Φcps α Variable ->
  DC.Δcps α ->
  Φcis α Variable ->
  DC.Δcis α ->
  TermX α Variable ->
  DT.Diff α ->
  DL.Diff
    (α, Binder Variable, TypeX α Variable)
    (D3.Diff (DA.Diff α) (DA.Diff (Binder Variable)) (DT.Diff α)) ->
  Maybe
    ( DL.Diff
        (α, Binder Variable, TypeX α Variable)
        (D3.Diff (DA.Diff α) (DA.Diff (Binder Variable)) (DT.Diff α))
    )
δconcatMapAddRecursiveMotive
  n
  δn
  ips
  δips
  iis
  δiis
  _cn
  _δcn
  cps
  δcps
  _cis
  _δcis
  motive
  δmotive
  δ = do
    let eff =
          δListFoldConcatMapAddRecursiveMotive
            n
            δn
            ips
            δips
            iis
            δiis
            motive
            δmotive
    case run . ignoreTrace $ runError eff of
      Left (_ :: String) -> Nothing
      Right δListFold -> δListFoldRight δListFold cps δcps (Just δ)

δmkCase ::
  ( Default α,
    Eq α,
    -- , PrettyPrintable α
    Show α
  ) =>
  Variable ->
  DA.Diff Variable ->
  Φips α Variable ->
  DI.Δips α ->
  Φiis α Variable ->
  DI.Δiis α ->
  Variable ->
  DA.Diff Variable ->
  Φcps α Variable ->
  DC.Δcps α ->
  Φcis α Variable ->
  DC.Δcis α ->
  DT.Diff α ->
  DT.Diff α ->
  Maybe (DT.Diff α)
δmkCase n δn ips δips iis δiis cn δcn cps δcps cis δcis δmotive =
  let addRec = addRecursiveMotive n ips iis motive
   in let hyps = concatMap addRec cps
       in case δconcatMapAddRecursiveMotive
            n
            δn
            ips
            δips
            iis
            δiis
            cn
            δcn
            cps
            δcps
            cis
            δcis
            motive
            δmotive
            DL.Same of
            Nothing -> const Nothing
            Just δhyps ->
              Just
                <$> δquantifyBinders hyps δhyps
                  . δapplyTerms
                    [(error "TODO", applyBinders cps (Var Nothing cn))]
                    (DL.Modify (D2.Modify DA.Same (δapplyBinders cps δcps (DT.CpyVar δcn))) DL.Same)
                  . δapplyTerms cis δcis

-- δListFoldMkPiConstructor ::
-- α -> Variable ->
-- Φips α Variable -> Φiis α Variable -> TermX α Variable ->
-- ΔListFold
-- (Variable, [(Variable, TypeX α Variable)], [TypeX α Variable])
-- (D2.Diff (DA.Diff Variable) (DT.Diff α)) (DT.Diff α)
δListFoldMkPiConstructor ::
  ( Default α,
    Eq α,
    -- , PrettyPrintable α
    Show α
  ) =>
  α ->
  Variable ->
  DA.Diff Variable ->
  Φips α Variable ->
  DI.Δips α ->
  Φiis α Variable ->
  DI.Δiis α ->
  TermX α Variable ->
  ΔListFold (Constructor α Variable) (DC.Diff α) (Maybe (DT.Diff α))
δListFoldMkPiConstructor α n δn ips δips iis δiis motive =
  δListFoldMkPiGenericMaybe pi δpi
  where
    pi (Constructor _ consName consParameters consIndices) =
      ( α,
        DT.Replace
          ( mkCase
              α
              n
              ips
              iis
              consName
              consParameters
              consIndices
              motive
          ),
        Binder Nothing
      )
    δpi (Constructor _ cn cps cis) = \case
      DC.Same -> Just (DT.Same, DA.Same)
      DC.Modify δcn δcps δcis -> do
        δcase <- δmkCase n δn ips δips iis δiis cn δcn cps δcps cis δcis DT.Same DT.Same
        return (δcase, DA.Same)

δquantifyCases ::
  ( Default α,
    Eq α,
    -- , PrettyPrintable α
    Show α
  ) =>
  α ->
  Variable ->
  DA.Diff Variable ->
  Φips α Variable ->
  DI.Δips α ->
  Φiis α Variable ->
  DI.Δiis α ->
  TermX α Variable ->
  [Constructor α Variable] ->
  DL.Diff (Constructor α Variable) (DC.Diff α) ->
  DT.Diff α ->
  Maybe (DT.Diff α)
δquantifyCases α n δn ips δips iis δiis motive cs δcs δ =
  δListFoldRight (δListFoldMkPiConstructor α n δn ips δips iis δiis motive) cs δcs (Just δ)

δmkDiscrimineeType ::
  ( Eq α
  -- , PrettyPrintable α
  ) =>
  DA.Diff Variable ->
  Φips α Variable ->
  DI.Δips α ->
  Φiis α Variable ->
  DI.Δiis α ->
  DT.Diff α
δmkDiscrimineeType δn ips δips iis δiis =
  δapplyBinders iis δiis $
    δapplyVariables ips δips $
      DT.CpyVar δn

δmkEliminatorType' ::
  Default α =>
  Eq α =>
  -- , PrettyPrintable α
  Show α =>
  α ->
  Variable ->
  DA.Diff Variable ->
  Φips α Variable ->
  DI.Δips α ->
  Φiis α Variable ->
  DI.Δiis α ->
  [Constructor α Variable] ->
  DI.Δcs α ->
  Maybe (DT.Diff α)
δmkEliminatorType' α n δn ips δips iis δiis cs δcs =
  let δdiscrimineeType = δmkDiscrimineeType δn ips δips iis δiis
   in let δmotiveType = δmkMotiveType' δn ips δips iis δiis
       in δquantifyVariables ips δips
            . DT.CpyPi δmotiveType DA.Same
            <$> ( -- quantifyCases may fail
                  δquantifyCases α n δn ips δips iis δiis motive cs δcs
                    --(unpackConstructors cs) (unpackΔConstructors δcs)
                    . δquantifyBinders iis δiis
                    . DT.CpyPi δdiscrimineeType DA.Same
                    . DT.CpyApp (δapplyBinders iis δiis DT.Same)
                    $ DT.Same
                )

δmkEliminatorType ::
  ( Default α,
    Eq α,
    -- , PrettyPrintable α
    Show α
  ) =>
  α ->
  Inductive α Variable ->
  DI.Diff α ->
  Maybe (DT.Diff α)
δmkEliminatorType α (Inductive n ips iis _u cs) δi = case δi of
  DI.Same -> Just DT.Same
  DI.Modify δn δips δiis _δu δcs ->
    δmkEliminatorType' α n δn ips δips iis δiis cs δcs

δeliminatorName :: DI.Diff α -> DA.Diff Variable
δeliminatorName = \case
  DI.Same -> DA.Same
  DI.Modify δn _ _ _ _ -> case δn of
    DA.Same -> DA.Same
    DA.Replace r -> DA.Replace $ mkEliminatorName r
