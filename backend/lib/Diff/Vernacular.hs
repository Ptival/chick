{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Diff.Vernacular
  ( Diff(..)
  , patch
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace
import           Data.Text.Prettyprint.Doc
import           Text.Printf

import qualified Definition as D
import           DefinitionObjectKind (DefinitionObjectKind)
import qualified Diff.Atom as DA
import qualified Diff.Inductive as DI
import qualified Diff.Term as DT
import           Diff.Utils
import qualified Inductive.Inductive as I
import           PrettyPrinting.PrettyPrintable
import qualified Term.Raw as Raw
import           Term.Term
import           Term.Universe
import           Vernacular

data Diff α
  = Same
  | ModifyDefinition (DA.Diff DefinitionObjectKind) (DA.Diff Variable) (DT.Diff α) (DT.Diff α)
  | ModifyInductive (DI.Diff α)
  | Replace (Vernacular α Variable)
  deriving (Show)

instance
  ( PrettyPrintable l α
  , PrettyPrintable l (α, Binder Variable, TermX α Variable)
  , PrettyPrintable l (α, TypeX α Variable)
  , PrettyPrintable l (α, Variable, TermX α Variable)
  , PrettyPrintable l (Binder Variable)
  , PrettyPrintable l (Branch α Variable)
  , PrettyPrintable l DefinitionObjectKind
  , PrettyPrintable l (I.Constructor α Variable)
  , PrettyPrintable l (TermX α Variable)
  , PrettyPrintable l Universe
  , PrettyPrintable l Variable
  , PrettyPrintable l (Vernacular α Variable)
  ) => PrettyPrintable l (Diff α) where
  prettyDoc = \case
    Same -> "Same"
    ModifyDefinition δ1 δ2 δ3 δ4 ->
      fillSep [ "ModifyDefinition", go δ1, go δ2, go δ3, go δ4 ]
    ModifyInductive δ1 -> fillSep [ "ModifyInductive", go δ1 ]
    Replace r -> fillSep [ "Replace", go r ]

    where
      go :: PrettyPrintable l a => a -> Doc ()
      go = parens . prettyDoc @l

patch ::
  ( Member (Exc String) r
  , Member Trace r
  ) =>
  Vernacular Raw.Raw Variable -> Diff Raw.Raw -> Eff r (Vernacular Raw.Raw Variable)
patch v δv =
  let exc (reason :: String) = throwExc $ printf "Diff.Vernacular/patch: %s" reason in
  case δv of
    Same -> return v
    ModifyDefinition δk δn δτ δt ->
      case v of
        Definition d -> do
          d' <- D.Definition
                <$> DA.patch (D.definitionKind d) δk
                <*> DA.patch (D.definitionName d) δn
                <*> DT.patch (D.definitionType d) δτ
                <*> DT.patch (D.definitionTerm d) δt
          return $ Definition d'
        _ -> exc "ModifyDefinition: not a Definition"
    ModifyInductive δind ->
      case v of
        Inductive ind -> Inductive <$> DI.patch ind δind
        _ -> exc "ModifyInductive: not an Inductive"
    Replace r -> return r
