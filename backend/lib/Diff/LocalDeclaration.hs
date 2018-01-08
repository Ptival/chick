{-# language FlexibleContexts #-}
{-# language LambdaCase #-}

module Diff.LocalDeclaration
  ( Diff(..)
  , patch
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Text.PrettyPrint.Annotated.WL

import qualified Diff.Atom as DA
import qualified Diff.Term as DT
import           PrettyPrinting.PrettyPrintable
import           Term.Binder
import           Term.Variable
--import           Typing.LocalContext
import           Typing.LocalDeclaration
import           Diff.Utils

data Diff α
  = Same
  | ModifyLocalAssum (DA.Diff (Binder Variable)) (DT.Diff α)
  -- for now, I don't need a diff for the term
  | ModifyLocalDef (DA.Diff Variable) (DT.Diff α)
  deriving (Show)

instance PrettyPrintable (Diff α) where
  prettyDoc = \case
    Same -> text "Same"
    ModifyLocalAssum δ1 δ2 ->
      fillSep [ text "ModifyLocalAssum", prettyDoc δ1, prettyDoc δ2 ]
    ModifyLocalDef δ1 δ2 ->
      fillSep [ text "ModifyLocalDef", prettyDoc δ1, prettyDoc δ2 ]

patch ::
  ( Member (Exc String) r
  , Member Trace r
  , Show α
  ) =>
  LocalDeclaration α Variable -> Diff α -> Eff r (LocalDeclaration α Variable)
patch ld δ = case (ld, δ) of

  (_, Same) -> return ld

  (LocalAssum b τ, ModifyLocalAssum δb δτ) ->
    LocalAssum
    <$> DA.patch b δb
    <*> DT.patch τ δτ

  (LocalDef v τ t, ModifyLocalDef δv δτ) ->
    LocalDef
    <$> DA.patch v δv
    <*> DT.patch τ δτ
    <*> return t

  (_, _) -> throwExc "Diff.LocalDeclaration/patch: mismatch"
