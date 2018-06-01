{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Diff.GlobalDeclaration
  ( Diff(..)
  , patch
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Data.Text.Prettyprint.Doc
import           Text.Printf

import qualified Diff.Atom as DA
import qualified Diff.Inductive as DI
import qualified Diff.Term as DT
import           Diff.Utils
import           Inductive.Inductive
import           PrettyPrinting.PrettyPrintable
import           Term.Term
import           Term.Universe
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

instance
  ( PrettyPrintable l α
  , PrettyPrintable l (α, Binder Variable, TermX α Variable)
  , PrettyPrintable l (α, TypeX α Variable)
  , PrettyPrintable l (α, Variable, TermX α Variable)
  , PrettyPrintable l (Binder Variable)
  , PrettyPrintable l (Branch α Variable)
  , PrettyPrintable l (Constructor α Variable)
  , PrettyPrintable l (TermX α Variable)
  , PrettyPrintable l Universe
  , PrettyPrintable l Variable
  ) => PrettyPrintable l (Diff α) where
  prettyDoc = \case
    Same -> "Same"
    ModifyGlobalAssum δ1 δ2    -> fillSep [ "ModifyGlobalAssum", go δ1, go δ2 ]
    ModifyGlobalDef   δ1 δ2 δ3 -> fillSep [ "ModifyGlobalDef",   go δ1, go δ2, go δ3 ]
    ModifyGlobalInd   δ1       -> fillSep [ "ModifyGlobalInd",   go δ1 ]
    where
      go :: PrettyPrintable l a => a -> Doc ()
      go = parens . prettyDoc @l

patch ::
  ( Member (Exc String) r
  , Member Trace r
  -- , PrettyPrintable l α
  , Show α
  ) => GlobalDeclaration α Variable -> Diff α -> Eff r (GlobalDeclaration α Variable)
patch gd δgd =
  let exc (reason :: String) = throwExc $ printf "Diff.GlobalDeclaration/patch: %s" reason in
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
