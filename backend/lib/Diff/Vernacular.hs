{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Diff.Vernacular
  ( Diff(..)
  , patch
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Text.PrettyPrint.Annotated.WL
import           Text.Printf

import qualified Diff.Atom as DA
import qualified Diff.Inductive as DI
import qualified Diff.Term as DT
import           Diff.Utils
import           PrettyPrinting.PrettyPrintable
import qualified Term.Raw as Raw
import           Term.Variable
import           Vernacular

data Diff α
  = Same
  | ModifyDefinition (DA.Diff Bool) (DA.Diff Variable) (DT.Diff α) (DT.Diff α)
  | ModifyInductive (DI.Diff α)
  deriving (Show)

instance PrettyPrintable α => PrettyPrintable (Diff α) where
  prettyDoc = \case
    Same                      -> text "Same"
    ModifyDefinition δ1 δ2 δ3 δ4 ->
      fillSep [ text "ModifyDefinition", go δ1, go δ2, go δ3, go δ4 ]
    ModifyInductive  δ1       -> fillSep [ text "ModifyInductive",  go δ1 ]

    where
      go :: PrettyPrintable a => a -> Doc ()
      go = parens . prettyDoc

patch ::
  ( Member (Exc String) r
  , Member Trace r
  ) =>
  Vernacular Raw.Raw Variable -> Diff Raw.Raw -> Eff r (Vernacular Raw.Raw Variable)
patch v δv =
  let exc reason = throwExc $ printf "Diff.Vernacular/patch: %s" reason in
  case δv of
    Same -> return v
    ModifyDefinition δb δn δτ δt ->
      case v of
        Definition b n τ t ->
          Definition
          <$> DA.patch b δb
          <*> DA.patch n δn
          <*> DT.patch τ δτ
          <*> DT.patch t δt
        _ -> exc "ModifyDefinition: not a Definition"
    ModifyInductive δind ->
      case v of
        Inductive ind -> Inductive <$> DI.patch ind δind
        _ -> exc "ModifyInductive: not an Inductive"
