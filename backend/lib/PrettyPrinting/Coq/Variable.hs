{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module PrettyPrinting.Coq.Variable
  (
  ) where

import Text.PrettyPrint.Annotated.WL

import Language (Language(Coq))
import PrettyPrinting.PrettyPrintable
import Term.Variable

instance PrettyPrintable 'Coq Variable where
  prettyDoc = text . unVariable
