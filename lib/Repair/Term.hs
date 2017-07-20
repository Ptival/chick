{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Repair.Term where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Text.Printf

import qualified Diff.Atom as DA
-- import qualified Diff.LocalContext as DLC
import qualified Diff.LocalDeclaration as DLD
import qualified Diff.List as DL
import qualified Diff.Term as DT
import           Diff.Utils
import           PrettyPrinting.PrettyPrintable
import qualified Repair.State as RS
import           Repair.Utils
import           Term.Binder
import           Term.Term
import qualified Term.Raw as Raw
import           Term.Variable
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

-- | `repairArgs τ δτ`
-- | `τ`  is the type of the remaining Pi-telescope
-- | `δτ` is the diff for `τ` to become the new telescope
repairArgs ::
  ( Member (Exc String) r
  , Member Trace r
  ) =>
  Raw.Term Variable -> DT.Diff Raw.Raw -> Eff r (DT.Diff Raw.Raw)
repairArgs = go DT.Same

  where
    go acc τ δτ =

      -- (do
      --   trace $ printf "go: %s" (prettyStrU τ)
      --   τ' <- DT.patch τ δτ
      --   trace $ printf "δτ: %s" (prettyStrU τ')
      -- )
      -- >>

      case δτ of
      DT.Same -> return acc
      DT.CpyPi _ _ d2    -> do
        (_, _, _, τ2) <- DT.extractPi τ
        go (DT.CpyApp acc DT.Same) τ2 d2
      DT.InsPi _ d1 _ d2 -> do
        τ2 <- DT.patch τ d2
        go (DT.InsApp () acc (hole d1)) τ2 d2
      _ -> throwExc "repairArgs: TODO"
    hole = \case
      DT.Change τ' -> DT.Change (Annot () (Hole ()) τ')
      _            -> DT.Change (Hole ())

-- | `repair t τ δτ` assumes `t` is a term whose type is `τ` and `δτ` is a diff describing
-- | how `τ` changed.  It attempts to build a patch `δt` s.t. `patch t δt` has type `patch τ δτ`.
repair ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RS.RepairState) r
  ) =>
  Raw.Term Variable -> Raw.Type Variable -> DT.Diff Raw.Raw -> Eff r (DT.Diff Raw.Raw)
repair t τ δτ =
  let exc (reason :: String) = throwExc $ printf "Repair.Term/repair: %s" reason in

  -- (do
  --     s <- get
  --     let γ = view RS.context s
  --     trace $ printf "CONTEXT BEF:\n%s" (prettyStrU γ)
  --     γ' <- DLC.patch γ (view RS.contextDiff s)
  --     trace $ printf "CONTEXT AFT:\n%s" (prettyStrU γ')
  -- ) >>

  case δτ of

  -- even though the type has not changed, the term might still need updating to deal with the
  -- changes in the context
  DT.Same                ->
    case t of
      -- f x y z   is   (((f x) y) z)
      App _ _ _ -> do
        -- (f, [x, y, z])
        (fun, _args)   <- DT.extractApps t
        case fun of

          Var _ v -> do
            -- s <- get
            -- let γ  = view RS.context s
            -- let δγ = view RS.δcontext s
            -- γ' <- DLC.patch γ δγ
            τv <- lookupType v
            δτv <- findDeclarationDiff v
            -- trace $ printf "About to update args with: %s" (show δτ)
            repairArgs τv δτv

          _ -> exc "repair, Same, App, Not Var"

      -- even though the diff is same, something inside might need updating
      Lam _ bt -> do
        let (b, tlam) = unscopeTerm bt
        (_, τ1, _, τ2) <- DT.extractPi τ
        withState
          (   over RS.context  (LC.addLocalAssum (b, τ1))
          >>> over RS.δcontext (DL.Keep)
          ) $ do
          DT.CpyLam DA.Same <$> repair tlam τ2 DT.Same

      _ -> do
        -- s :: RS.State <- get
        -- trace $ printf "SAME:        %s" (prettyStrU t)
        -- let γ = view RS.context s
        -- trace $ printf "CONTEXT BEF:\n%s" (prettyStrU γ)
        -- γ' <- DLC.patch γ (view RS.contextDiff s)
        -- trace $ printf "CONTEXT AFT:\n%s" (prettyStrU γ')
        return DT.Same

  DT.Change τ'      -> return $ DT.Change $ Annot () (Hole ()) τ'

  DT.CpyApp _ _     -> throwExc "repair: CpyApp"
  DT.CpyLam _ _     -> throwExc "repair: CpyLam"
  DT.InsApp _ _ _   -> throwExc "repair: InsApp"
  DT.InsLam _ _ _   -> throwExc "repair: InsLam"
  DT.PermutLams _ _ -> throwExc "repair: PermutLams"

  DT.CpyPi d1 DA.Same d2 ->
    case (t, τ) of
      (Lam _ bt, Pi _ τ1 bτ2) -> do
        let (b, _) = unscopeTerm bt
        withState
          (   over RS.context  (LC.addLocalAssum (b, τ1))
          >>> over RS.δcontext (DL.Change (DLD.Change DA.Same d1))
          ) $ do
          DT.CpyLam DA.Same <$> repair (snd $ unscopeTerm bt) (snd $ unscopeTerm bτ2) d2
      _ -> throwExc "repair: CpyPi Same"

  DT.CpyPi _ _ _         -> throwExc "repair: CpyPi"

  DT.InsPi _ d1 _b d2      -> do
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
    τ1' <- DT.patch τ d1
    withState
      (   over RS.context  id
      >>> over RS.δcontext (DL.Insert (LocalAssum v τ1'))
      ) $ DT.InsLam () b <$> repair t τ d2

  DT.PermutPis p d1      -> do
    (pis, τrest) <- DT.extractSomePis (length p) τ
    let pis' = permute p pis
    (lams, trest) <- DT.extractSomeLams (length p) t -- TODO: catchError and try something else?
    let lams' = permute p lams
    DT.PermutLams p <$> repair (DT.mkLams lams' trest) (DT.mkPis pis' τrest) d1
