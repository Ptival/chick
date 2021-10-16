{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Chick.Vernacular where

import Control.Monad.Reader (runReader)
import Data.Default (Default (def))
-- import Prettyprinter

import Language (Language (Chick))
import Language.OCaml.PrettyPrinter (pretty)
import PrettyPrinting.Chick.Definition ()
import PrettyPrinting.Chick.Inductive ()
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyDoc),
  )
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (prettyDocU),
  )
import PrettyPrinting.Term ()
import Term.Variable (Variable)
import Vernacular (Vernacular (..))

instance PrettyPrintableUnannotated 'Chick (Vernacular α Variable) where
  prettyDocU = \case
    Definition d -> prettyDocU @'Chick d
    Inductive i -> prettyDocU @'Chick i
    Vernacular.UnsupportedOCaml o ->
      return $ Language.OCaml.PrettyPrinter.pretty o

instance PrettyPrintable 'Chick (Vernacular α Variable) where
  prettyDoc v = runReader (prettyDocU @'Chick v) def
