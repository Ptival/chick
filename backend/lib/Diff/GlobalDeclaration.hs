{-# language FlexibleContexts #-}
{-# language LambdaCase #-}

module Diff.GlobalDeclaration
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
import           Term.Variable
import           Typing.GlobalDeclaration

data Diff α
  = Same
  | ModifyGlobalAssum (DA.Diff Variable) (DT.Diff α)
  | ModifyGlobalDef   (DA.Diff Variable) (DT.Diff α) (DT.Diff α)
  | ModifyGlobalInd   (DI.Diff α)

instance Show α => Show (Diff α) where
  show Same = "Same"
  show (ModifyGlobalAssum δn δτ)    = printf "ModifyGlobalAssum %s %s"  (show δn) (show δτ)
  show (ModifyGlobalDef   δn δτ δt) = printf "ModifyGlobalDef %s %s %s" (show δn) (show δτ) (show δt)
  show (ModifyGlobalInd   δind)     = printf "ModifyGlobalInd %s"       (show δind)

instance PrettyPrintable α => PrettyPrintable (Diff α) where
  prettyDoc = \case
    Same -> text "Same"
    ModifyGlobalAssum δ1 δ2    -> fillSep [ text "ModifyGlobalAssum", go δ1, go δ2 ]
    ModifyGlobalDef   δ1 δ2 δ3 -> fillSep [ text "ModifyGlobalDef",   go δ1, go δ2, go δ3 ]
    ModifyGlobalInd   δ1       -> fillSep [ text "ModifyGlobalInd",   go δ1 ]
    where
      go :: PrettyPrintable a => a -> Doc ()
      go = parens . prettyDoc

patch ::
  ( Member (Exc String) r
  , Member Trace r
  , PrettyPrintable α
  , Show α
  ) => GlobalDeclaration α Variable -> Diff α -> Eff r (GlobalDeclaration α Variable)
patch gd δgd =
  let exc reason = throwExc $ printf "Diff.GlobalDeclaration/patch: %s" reason in
  case δgd of

    Same -> return gd

    ModifyGlobalAssum δv δτ ->
      case gd of
        GlobalAssum v τ -> GlobalAssum <$> DA.patch v δv <*> DT.patch τ δτ
        _               -> exc $ "ModifyGlobalAssum: not a GlobalAssum"

    ModifyGlobalDef δv δτ δt ->
      case gd of
        GlobalDef v τ t -> GlobalDef <$> DA.patch v δv <*> DT.patch τ δτ <*> DT.patch t δt
        _               -> exc $ "ModifyGlobalDef: not a GlobalDef"

    ModifyGlobalInd δind ->
      case gd of
        GlobalInd ind -> GlobalInd <$> DI.patch ind δind
        _               -> exc $ "ModifyGlobalInd: not a GlobalInd"
