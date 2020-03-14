{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module PrettyPrinting.OCaml.Variable
  (
  ) where

import Text.PrettyPrint.Annotated.WL

import Language (Language(OCaml))
import PrettyPrinting.PrettyPrintable
import Term.Variable

instance PrettyPrintable 'OCaml Variable where
  prettyDoc = text . unVariable
