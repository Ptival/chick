{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repair.Term where

import           Control.Arrow
import           Control.Lens hiding (preview)
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Text.Printf

import qualified Diff.Atom as ΔA
-- import qualified Diff.GlobalDeclaration as ΔGD
-- import qualified Diff.LocalContext as ΔLC
import qualified Diff.LocalDeclaration as ΔLD
import qualified Diff.List as ΔL
import qualified Diff.Term as ΔT
import           Diff.Utils
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import qualified Repair.State as RS
import           Repair.Utils
import           Term.Binder
import           Term.Term
-- import qualified Term.TypeChecked as C
import qualified Term.Raw as Raw
import qualified Term.Universe as U
import qualified Typing.LocalContext as LC
import           Typing.LocalDeclaration
import           Utils

-- | For example:
-- | τ  = A → C → D                             t  = (f a) c
-- | τ' = A → (B → (C -> D))                    t' = ((f a) _) c
-- | Δ  = CpyP Same (InsP B (CpyP Same Same))   δ  = CpyA (InsA (CpyA Same Same) InsH) Same
-- |      ^^^^                                                   ^^^^
-- |                 ^^^^                                  ^^^^
-- |                         ^^^^                    ^^^^

-- | `repairArgs args τ δτ δfun`
-- | `args` is the list of arguments prior to changes
-- | `τ`    is the type of the remaining Pi-telescope
-- | `δτ`   is the diff for `τ` to become the new telescope
-- | `δfun` is the diff to apply to the actual function, which is the base of this "fold"
repairArgs ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RS.RepairState) r
  ) =>
  [Raw.Term Variable] ->
  Raw.Type Variable ->
  ΔT.Diff Raw.Raw ->
  ΔT.Diff Raw.Raw ->
  Eff r (ΔT.Diff Raw.Raw)
repairArgs args τ0 δτ0 δfun =
  trace "Repair.Term/repairArgs with:" >>
  trace (printf "> args: %s" (prettyStr args)) >>
  trace (printf "> τ0:   %s" (prettyStr   τ0)) >>
  trace (printf "> δτ0:  %s" (prettyStr  δτ0)) >>
  trace (printf "> δfun: %s" (prettyStr δfun)) >>
  go δfun args τ0 δτ0
  where

    exc (reason :: String) = throwExc $ printf "Repair.Term/repairArgs: %s" reason

    -- go accumulates the resulting diff in an unintuitive way
    go acc args τ δτ =

      (do
        trace $ "Repair.Term/repairArgs/go:"
        trace $ printf "> args: %s" (prettyStr args)
        trace $ printf "> τ:    %s" (prettyStr τ)
        trace $ printf "> δτ:   %s" (prettyStr δτ)
        τ' <- ΔT.patch τ δτ
        trace $ printf "> τ':   %s" (prettyStr τ')
      )
      >>

      case (args, δτ) of

        ([], ΔT.CpyApp _ _) -> return acc

        --        | TYPE             | TERM
        -- BEFORE | (x : X) → Ys → R | acc x ys
        -- AFTER  | (x : X) → Zs → R | acc x zs
        (arg : args, ΔT.CpyPi δ1 _ δ2) -> do
          (_, τ1, _, τ2) <- ΔT.extractPi τ
          δarg <- repair arg τ1 δ1
          go (ΔT.CpyApp acc δarg) args τ2 δ2

        ([], ΔT.CpyPi _ _ _) ->
          -- this happens when the function was partially applied
          return acc

        ([], ΔT.CpyVar _) -> return acc

        ([], ΔT.InsApp _ _ _) -> return acc -- TODO: can we do better here?

        --        | TYPE                  | TERM
        -- BEFORE | Xs →         → Zs → R | f xs zs
        -- AFTER  | Xs → (y : Y) → Zs → R | f xs y zs
        (_, ΔT.InsPi _ δ1 _ δ2) ->
          go (ΔT.InsApp () acc (hole δ1)) args τ δ2

        --        | TYPE             | TERM
        -- BEFORE | A1 → A2 → Bs → R | acc a1 a2 bs
        -- AFTER  | A2 → A1 → Bs → R | acc a2 a1 bs
        (_, ΔT.PermutPis p δτ') -> do
          let args' = permute p args
          ΔT.PermutApps p <$> go acc args' τ δτ'
        -- TODO: this is wrong because we need the permutations to happen from within the
        -- innermost App rather than the outermost ones

        (_, ΔT.Same) -> do
          -- even though it's Same, we still need to peel off ∀s from τ
          -- before returning acc
          trace $ printf "τ: %s" (prettyStr τ)
          case (args, τ) of
            (_ : args, Pi _ _ bτ') ->
              let (_, τ') = unscopeTerm bτ' in
              go (ΔT.CpyApp acc ΔT.Same) args τ' ΔT.Same
            _         -> return acc

        _ -> exc $ printf "repairArgs, TODO:\nargs: %s\nτ: %s\nδτ: %s"
             (show args) (show τ) (show δτ)

    hole = \case
      ΔT.Replace τ' -> ΔT.Replace (Annot () (Hole ()) τ')
      _             -> ΔT.Replace (Hole ())

-- | `genericRepair t τ` attempts to repair `t` without more information about
-- | how its type `τ` changed
genericRepair ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RS.RepairState) r
  ) =>
  Raw.Term Variable -> Raw.Type Variable -> Eff r (ΔT.Diff Raw.Raw)
genericRepair t τ = do

  let exc (reason :: String) =
        throwExc $ printf "Repair.Term/genericRepair: %s" reason

  trace $ printf "Repair.Term/genericRepair(t: %s, τ: %s)"
    (prettyStrU t) (prettyStrU τ)

  case t of

    -- f x y z   is   (((f x) y) z)
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
          trace $ printf "Trying to repair an application of %s" (prettyStr v)
          trace $ printf "Looked up type: %s" (prettyStr τv)
          (δv, δτv) <- unpackDeclarationDiff <$> findDeclarationDiff v
          trace $ printf "τv: %s" (prettyStr τv)
          trace $ printf "δτv: %s" (prettyStr δτv)
          repairArgs (map snd args) τv δτv (ΔT.CpyVar δv)

        _ -> exc "repair, Same, App, Not Var"

    -- even though the diff is same, something inside might need updating
    Lam _ bt -> do
      let (b, tlam) = unscopeTerm bt
      (_, τ1, _, τ2) <- ΔT.extractPi τ
      withState
        (   over RS.context  (LC.addLocalAssum (b, τ1))
        >>> over RS.δcontext (ΔL.Keep)
        ) $ do
        ΔT.CpyLam ΔA.Same <$> repair tlam τ2 ΔT.Same

    Pi _ τ1 bτ2 -> do
      let (b, τ2) = unscopeTerm bτ2
      withState
        (   over RS.context  (LC.addLocalAssum (b, τ1))
        >>> over RS.δcontext (ΔL.Keep)
        ) $ do
        ΔT.CpyPi
          <$> repair τ1 (Type U.Type) ΔT.Same
          <*> pure ΔA.Same
          <*> repair τ2 (Type U.Type) ΔT.Same

    _ -> do
      -- s :: RS.State <- get
      -- trace $ printf "SAME:        %s" (prettyStrU t)
      -- let γ = view RS.context s
      -- trace $ printf "CONTEXT BEF:\n%s" (prettyStrU γ)
      -- γ' <- ΔLC.patch γ (view RS.contextDiff s)
      -- trace $ printf "CONTEXT AFT:\n%s" (prettyStrU γ')
      return ΔT.Same

-- | `repair t τ δτ` assumes `t` is a term whose type is `τ` and `δτ` is a diff describing
-- | how `τ` changed.  It attempts to build a patch `δt` s.t. `patch t δt` has type `patch τ δτ`.
repair ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RS.RepairState) r
  ) =>
  Raw.Term Variable -> Raw.Type Variable -> ΔT.Diff Raw.Raw -> Eff r (ΔT.Diff Raw.Raw)
repair t τ δτ =
  trace (printf "Repair.Term/repair(t: %s, τ: %s, δτ: %s)" (prettyStrU t) (prettyStrU τ) (show δτ)) >>
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
  ΔT.Same -> genericRepair t τ

  ΔT.Replace τ' -> return $ ΔT.Replace $ Annot () (Hole ()) τ'

  ΔT.CpyApp _ _ -> genericRepair t τ

  ΔT.CpyLam _ _ -> exc "CpyLam"

  ΔT.CpyVar ΔA.Same -> exc "CpyVar Same"

  ΔT.CpyVar (ΔA.Replace δv) -> do
    trace $ printf "AT THIS POINT δv IS: %s, t IS: %s" (preview δv) (preview t)
    _ <- exc "YO FIXME"
    return $ ΔT.Replace "TODO" -- TODO: confirm this is always good

  ΔT.InsApp _ _ _ -> genericRepair t τ -- not sure what to do here

  ΔT.InsLam _ _ _   -> exc "InsLam"
  ΔT.PermutLams _ _ -> exc "PermutLams"
  ΔT.PermutApps _ _ -> exc "PermutApps"

  ΔT.CpyPi d1 δb d2 ->
    case (t, τ) of
      (Lam _ bt, Pi _ τ1 bτ2) -> do
        let (b, _) = unscopeTerm bt
        withState
          (   over RS.context  (LC.addLocalAssum (b, τ1))
          >>> over RS.δcontext (ΔL.Modify (ΔLD.ModifyLocalAssum δb d1))
          ) $ do
          ΔT.CpyLam ΔA.Same <$> repair (snd $ unscopeTerm bt) (snd $ unscopeTerm bτ2) d2
      _ -> exc "repair: CpyPi Same"

  ΔT.InsPi _ d1 _b d2      -> do
    -- I think what we want here is:
    -- - find a b' binder like b that is free in t
    -- - InsLam b'
    -- - recursively diff by substituting b' for b
    s <- get
    let varsFreeInTerm = foldr (\ v -> (v :)) [] t
    let varsBoundInContext = LC.boundNames (view RS.context s)
    trace $ printf "Variables free  in the term:    %s" (show . map prettyStr $ varsFreeInTerm)
    trace $ printf "Variables bound in the context: %s" (show . map prettyStr $ varsBoundInContext)
    let v :: Variable = "todo"
    let b = Binder (Just v)
    τ1' <- ΔT.patch τ d1
    withState
      (   over RS.context  id
      >>> over RS.δcontext (ΔL.Insert (LocalAssum (Binder (Just v)) τ1'))
      ) $ ΔT.InsLam () b <$> repair t τ d2

  ΔT.PermutPis p d1      -> do
    (pis, τrest) <- ΔT.extractSomePis (length p) τ
    let pis' = permute p pis
    (lams, trest) <- ΔT.extractSomeLams (length p) t -- TODO: catchError and try something else?
    let lams' = permute p lams
    ΔT.PermutLams p <$> repair (ΔT.mkLams lams' trest) (ΔT.mkPis pis' τrest) d1

  ΔT.RemovePi _ -> exc "RemovePi"
