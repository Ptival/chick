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
import           Term.Variable
--import           Typing.LocalContext
import           Typing.LocalDeclaration

data Diff α
  = Same
  | Modify
    (DA.Diff Variable) -- the name
    (DT.Diff α)        -- the type
  deriving (Show)

instance PrettyPrintable (Diff α) where
  prettyDoc = \case
    Same         -> text "Same"
    Modify δ1 δ2 -> fillSep [ text "Modify", prettyDoc δ1, prettyDoc δ2 ]

patch ::
  ( Member (Exc String) r
  , Member Trace r
  ) =>
  LocalDeclaration α Variable -> Diff α -> Eff r (LocalDeclaration α Variable)
patch ld Same = return ld
patch ld (Modify dv dτ) =
  case ld of
    LocalAssum v τ   -> LocalAssum <$> DA.patch v dv              <*> DT.patch τ dτ
    LocalDef   v τ t -> LocalDef   <$> DA.patch v dv <*> DT.patch τ dτ <*> return t
