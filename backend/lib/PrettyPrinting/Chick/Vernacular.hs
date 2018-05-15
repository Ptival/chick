{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module PrettyPrinting.Chick.Vernacular
  (
  ) where

import Control.Monad.Reader
import Data.Default
import Data.Text.Prettyprint.Doc
import Language.OCaml.PrettyPrinter

import Language (Language(Chick))
import PrettyPrinting.Chick.Definition ()
import PrettyPrinting.Chick.Inductive ()
import PrettyPrinting.Term ()
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import Term.Term
import Vernacular

instance PrettyPrintableUnannotated 'Chick (Vernacular α Variable) where
  prettyDocU = \case
    Definition d -> prettyDocU @'Chick d
    Inductive i -> prettyDocU @'Chick i
    Vernacular.UnsupportedOCaml o ->
      return $ Language.OCaml.PrettyPrinter.pretty o

instance PrettyPrintable 'Chick (Vernacular α Variable) where
  prettyDoc v = runReader (prettyDocU @'Chick v) def
