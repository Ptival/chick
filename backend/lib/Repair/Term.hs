{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module Repair.Term where

import           Control.Arrow
--import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Trace
import           Text.Printf

import qualified Diff.Atom as ΔA
--import qualified Diff.GlobalDeclaration as ΔGD
import qualified Diff.GlobalEnvironment as ΔGE
import qualified Diff.List as ΔL
import qualified Diff.Term as ΔT
import           Diff.Utils
import           Inductive.Inductive
import           Language (Language(Chick))
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import qualified Repair.State as RS
import           Repair.Term.Argument
import           Repair.Term.Branch
import           Repair.Utils
import           Term.Binder
import           Term.Term
-- import qualified Term.TypeChecked as C
import qualified Term.Raw as Raw
import qualified Term.Universe as U
import qualified Typing.GlobalEnvironment as GE

guessIndMatched ::
  ( RepairEff r
  ) =>
  Raw.Term Variable -> [Branch Raw.Raw Variable] -> Eff r (Inductive Raw.Raw Variable)
guessIndMatched discriminee _branches = do
  let exc (s :: String) = throwExc $ printf "Repair.Term/guessIndMatched: %s" s
  trace "*** Trying to guess matched type using discriminee"
  case discriminee of
    Var _ v -> do
      τv <- lookupType v
      (fun, _) <- ΔT.extractApps τv
      case fun of
        Var _ indName -> do
          (e, _) <- RS.getEnvironments
          trace $ printf "ENVIRONMENT:\n%s" (prettyStrU @'Chick e)
          case GE.lookupInductiveByName indName e of
            Just ind -> return ind
            Nothing -> exc $ printf "Could not find %s in global environment" (prettyStr @'Chick indName)
        _ -> exc "Could not guess the inductive being matched because the type of the discriminee did not look like a variable applied"
    _ -> exc "Could not guess the inductive being matched because the term being matched is not a variable (TODO)"

-- | `unknownTypeRepair t` attempts to repair `t` without any information
unknownTypeRepair ::
  ( RepairEff r
  ) =>
  Raw.Term Variable -> Eff r (ΔT.Diff Raw.Raw)
unknownTypeRepair t = do

  let exc (reason :: String) =
        throwExc $ printf "Repair.Term/unknownTypeRepair: %s" reason

  trace $ printf "Repair.Term/unknownTypeRepair(t: %s)" (prettyStrU @'Chick t)

  case t of

    Type _ -> return ΔT.Same

    Var _ v -> do
      τv <- lookupType v
      (δv, δτv) <- unpackDeclarationDiff <$> findDeclarationDiff v
      repairArgs repair [] τv δτv (ΔT.CpyVar δv)

    App _ _ _ -> do
      -- (f, [x, y, z])
      (fun, args)   <- ΔT.extractApps t
      case fun of

        Var _ v -> do
          -- s <- get
          -- let γ  = view RS.context s
          -- let δγ = view RS.δcontext s
          -- γ' <- ΔLC.patch γ δγ
          τv <- lookupType v
          trace $ printf "Trying to repair an application of %s" (prettyStr @'Chick v)
          trace $ printf "Looked up type: %s" (prettyStr @'Chick τv)
          (δv, δτv) <- unpackDeclarationDiff <$> findDeclarationDiff v
          trace $ printf "τv: %s" (prettyStr @'Chick τv)
          trace $ printf "δτv: %s" (prettyStr @'Chick δτv)
          repairArgs repair (map snd args) τv δτv (ΔT.CpyVar δv)

        _ -> exc "repair, Same, App, Not Var"

    Pi _ τ1 bτ2 -> do
      let (b, τ2) = unscopeTerm bτ2
      RS.withLocalAssum (b, τ1) >>> RS.withδLocalContext ΔL.Keep $ do
        ΔT.CpyPi
          <$> repair τ1 (Type U.Type) ΔT.Same
          <*> pure ΔA.Same
          <*> repair τ2 (Type U.Type) ΔT.Same

    Match _ d bs -> do
      δd <- unknownTypeRepair d
      -- eventually, it'd be nice to have the type at which the match is done already
      -- figured out
      ind  <- guessIndMatched d bs
      trace $ printf "*** INDUCTIVE MATCHED: %s" (prettyStr @'Chick ind)
      (e, δe) <- RS.getEnvironments
      δind <- ΔGE.findGlobalIndDiff ind e δe
      trace $ printf "*** δINDUCTIVE MATCHED: %s" (prettyStr @'Chick δind)
      δbs <- repairBranches unknownTypeRepair bs ind δind
      trace $ printf "*** PROPOSED: %s" (prettyStr @'Chick δbs)
      return $ ΔT.CpyMatch δd δbs

    Annot _ _ _ -> exc "utr: Annot"
    Hole _ -> exc "utr: Hole"
    Lam _ _ -> exc "utr: Lam"
    Let _ _ _ -> exc "utr: Let"

-- | `genericRepair t τ` attempts to repair `t` without more information about
-- | how its type `τ` changed
genericRepair ::
  ( RepairEff r
  ) =>
  Raw.Term Variable -> Raw.Type Variable -> Eff r (ΔT.Diff Raw.Raw)
genericRepair t τ = do

  -- let exc (reason :: String) =
  --       throwExc $ printf "Repair.Term/genericRepair: %s" reason

  trace $ printf "Repair.Term/genericRepair(t: %s, τ: %s)"
    (prettyStrU @'Chick t) (prettyStrU @'Chick τ)

  case t of

    -- even though the diff is same, something inside might need updating
    Lam _ bt -> do
      let (b, tlam) = unscopeTerm bt
      (_, τ1, _, τ2) <- ΔT.extractPi τ
      RS.withLocalAssum (b, τ1) >>> RS.withδLocalContext ΔL.Keep $ do
        ΔT.CpyLam ΔA.Same <$> repair tlam τ2 ΔT.Same

    _ -> unknownTypeRepair t

{-| `repair t τ δτ` assumes `t` is a term whose type is `τ` and `δτ` is a diff
describing how `τ` changed.  It attempts to build a patch `δt` s.t. `patch t δt`
has type `patch τ δτ`. |-}
repair ::
  ( RepairEff r
  ) =>
  Raw.Term Variable -> Raw.Type Variable -> ΔT.Diff Raw.Raw -> Eff r (ΔT.Diff Raw.Raw)
repair t τ δτ =
  trace (printf "Repair.Term/repair(t: %s, τ: %s, δτ: %s)" (prettyStrU @'Chick t) (prettyStrU @'Chick τ) (show δτ)) >>
  RS.traceState >>
  let exc (reason :: String) = throwExc $ printf "Repair.Term/repair: %s" reason in

  -- (do
  --     s <- get
  --     let γ = view RS.context s
  --     trace $ printf "CONTEXT BEF:\n%s" (prettyStrU γ)
  --     γ' <- ΔLC.patch γ (view RS.contextDiff s)
  --     trace $ printf "CONTEXT AFT:\n%s" (prettyStrU γ')
  -- ) >>

  case δτ of

  -- even though the type has not changed, the term might still need updating to
  -- deal with the changes in the context
  ΔT.Same -> genericRepair t τ -- OK

  ΔT.Replace τ' -> return $ ΔT.Replace $ Annot () (Hole ()) τ'

  ΔT.CpyApp _ _ -> genericRepair t τ -- FIXME improve this?

  ΔT.CpyLam _ _ -> genericRepair t τ -- FIXME: improve this

  ΔT.CpyMatch _ _ -> genericRepair t τ -- FIXME: improve this

  ΔT.CpyVar ΔA.Same -> genericRepair t τ -- OK

  ΔT.CpyVar (ΔA.Replace δv) -> do
    trace $ printf "AT THIS POINT δv IS: %s, t IS: %s" (preview @'Chick δv) (preview @'Chick t)
    _ <- exc "YO FIXME"
    return $ ΔT.Replace "TODO" -- TODO: confirm this is always good

  ΔT.InsApp _ _ _ -> genericRepair t τ -- not sure what to do here

  ΔT.InsLam _ _ _   -> genericRepair t τ -- FIXME: improve this
  ΔT.PermutLams _ _ -> genericRepair t τ -- FIXME: improve this
  ΔT.PermutApps _ _ -> genericRepair t τ -- FIXME: improve this

  ΔT.CpyPi δ1 δb δ2 ->
    case (t, τ) of
      (Lam _ bt, Pi _ τ1 bτ2) -> do
        let (b, _) = unscopeTerm bt
        RS.withLocalAssumAndδ (b, τ1) (δb, δ1) $ do
          ΔT.CpyLam ΔA.Same
            <$> repair (snd $ unscopeTerm bt) (snd $ unscopeTerm bτ2) δ2
      _ -> genericRepair t τ

  ΔT.InsPi _ d1 _b d2      -> do
    -- I think what we want here is:
    -- - find a b' binder like b that is free in t
    -- - InsLam b'
    -- - recursively diff by substituting b' for b
    let varsFreeInTerm = foldr (\ v -> (v :)) [] t
    boundVarsInContext <- RS.boundVarsInContext
    trace $ printf "Variables free  in the term:    %s" (show . map (prettyStr @'Chick) $ varsFreeInTerm)
    trace $ printf "Variables bound in the context: %s" (show . map (prettyStr @'Chick) $ boundVarsInContext)
    let v :: Variable = "TODO"
    let b = Binder (Just v)
    τ1' <- ΔT.patch τ d1
    RS.withInsertLocalAssum (Binder . Just $ v, τ1') $
      ΔT.InsLam () b <$> repair t τ d2

  ΔT.PermutPis p d1      -> do
    (pis, τrest) <- ΔT.extractSomePis (length p) τ
    let pis' = permute p pis
    (lams, trest) <- ΔT.extractSomeLams (length p) t -- TODO: catchError and try something else?
    let lams' = permute p lams
    ΔT.PermutLams p <$> repair (ΔT.mkLams lams' trest) (ΔT.mkPis pis' τrest) d1

  ΔT.RemoveApp _ -> genericRepair t τ -- FIXME: improve this

  ΔT.RemoveLam _ -> genericRepair t τ -- FIXME: improve this

  ΔT.RemovePi _ -> genericRepair t τ -- FIXME: improve this
