{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module PrettyPrinting.Chick.Variable
  (
  ) where

import Data.Text.Prettyprint.Doc

import Language (Language(Chick))
import PrettyPrinting.PrettyPrintable
import Term.Variable

instance PrettyPrintable 'Chick Variable where
  prettyDoc = pretty . unVariable
