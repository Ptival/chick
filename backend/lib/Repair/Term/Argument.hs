{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Repair.Term.Argument
  ( repairArgs
  ) where

import           Polysemy                       ( Sem )
import           Polysemy.Trace                 ( trace )
import           Text.Printf                    ( printf )

import qualified Diff.Term as ΔT
import           Diff.Utils
import           Language                       ( Language(Chick) )
import           PrettyPrinting.Chick           ( )
import           PrettyPrinting.PrettyPrintable
import           Repair.Utils
import           Term.Term
import qualified Term.Raw                       as Raw

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
  Repair r =>
  RepairTermType r ->
  [Raw.Term Variable] ->
  Raw.Type Variable ->
  ΔT.Diff Raw.Raw ->
  ΔT.Diff Raw.Raw ->
  Sem r (ΔT.Diff Raw.Raw)
repairArgs repair args τ0 δτ0 δfun =
  trace "Repair.Term/repairArgs with:" >>
  trace (printf "> args: %s" (prettyStr @'Chick args)) >>
  trace (printf "> τ0:   %s" (prettyStr @'Chick   τ0)) >>
  trace (printf "> δτ0:  %s" (prettyStr @'Chick  δτ0)) >>
  trace (printf "> δfun: %s" (prettyStr @'Chick δfun)) >>
  go δfun args τ0 δτ0
  where

    exc (reason :: String) = throwExc $ printf "Repair.Term/repairArgs: %s" reason

    -- go accumulates the resulting diff in an unintuitive way
    go acc args τ δτ =

      (do
        trace $ "Repair.Term/repairArgs/go:"
        trace $ printf "> args: %s" (prettyStr @'Chick args)
        trace $ printf "> τ:    %s" (prettyStr @'Chick τ)
        trace $ printf "> δτ:   %s" (prettyStr @'Chick δτ)
        τ' <- ΔT.patch τ δτ
        trace $ printf "> τ':   %s" (prettyStr @'Chick τ')
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

        --        | TYPE       | TERM
        -- BEFORE | A → Bs → R | acc a bs
        -- AFTER  |     Bs → R | acc bs
        (_ : args, ΔT.RemovePi δτ') -> do
          (_, _, _, τ2) <- ΔT.extractPi τ
          go (ΔT.RemoveApp acc) args τ2 δτ'

        (_, ΔT.Same) -> do
          -- even though it's Same, we still need to peel off ∀s from τ
          -- before returning acc
          trace $ printf "τ: %s" (prettyStr @'Chick τ)
          case (args, τ) of
            (_ : args, Pi _ _ bτ') ->
              let (_, τ') = unscopeTerm bτ' in
              go (ΔT.CpyApp acc ΔT.Same) args τ' ΔT.Same
            _         -> return acc

        (_, ΔT.Replace τ') -> do
          -- if this happens, we have no reason to believe that the remaining arguments
          -- are relevant to the replacement type: we need to drop them all, and then
          -- insert as many arguments as the replacement type calls for
          pis <- ΔT.extractPis τ'
          return
            . ΔT.nInsertApps (length pis) ((), Hole ())
            . ΔT.nRemoveApps (length args)
            $ acc

        _ -> exc $ printf "repairArgs, TODO:\nargs: %s\nτ: %s\nδτ: %s"
             (show args) (show τ) (show δτ)

    hole = \case
      ΔT.Replace τ' -> ΔT.Replace (Annot () (Hole ()) τ')
      _             -> ΔT.Replace (Hole ())
