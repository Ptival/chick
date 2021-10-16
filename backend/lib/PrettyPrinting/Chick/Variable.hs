{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Chick.Variable
  (
  )
where

import Language (Language (Chick))
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyDoc),
  )
import Prettyprinter (Pretty (pretty))
import Term.Variable (Variable (..))

instance PrettyPrintable 'Chick Variable where
  prettyDoc = pretty . unVariable
