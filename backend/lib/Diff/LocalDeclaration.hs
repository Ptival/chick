{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Diff.LocalDeclaration (
  Diff(..),
  patch,
  ) where

import qualified Data.Text.Prettyprint.Doc      as Doc
import           Polysemy                       ( Member, Sem )
import           Polysemy.Error                 ( Error )
import           Polysemy.Trace                 ( Trace )

import qualified Diff.Atom                      as DA
import qualified Diff.Term                      as DT
import           PrettyPrinting.PrettyPrintable
import           Term.Binder
import           Term.Term
import           Typing.LocalDeclaration
import           Diff.Utils

data Diff α
  = Same
  | ModifyLocalAssum (DA.Diff (Binder Variable)) (DT.Diff α)
  -- for now, I don't need a diff for the term
  | ModifyLocalDef (DA.Diff Variable) (DT.Diff α)
  deriving (Show)

instance
  ( PrettyPrintable l (Binder Variable)
  , PrettyPrintable l (Branch α Variable)
  , PrettyPrintable l (TermX α Variable)
  , PrettyPrintable l Variable
  ) => PrettyPrintable l (Diff α)
  where
  prettyDoc = \case
    Same -> "Same"
    ModifyLocalAssum δ1 δ2 ->
      Doc.fillSep [ "ModifyLocalAssum", prettyDoc @l δ1, prettyDoc @l δ2 ]
    ModifyLocalDef δ1 δ2 ->
      Doc.fillSep [ "ModifyLocalDef", prettyDoc @l δ1, prettyDoc @l δ2 ]

patch ::
  Member (Error String) r =>
  Member Trace          r =>
  Show α =>
  LocalDeclaration α Variable -> Diff α -> Sem r (LocalDeclaration α Variable)
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
