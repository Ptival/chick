{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Chick.Script
  (
  )
where

import Language (Language (Chick))
import PrettyPrinting.Chick.Vernacular ()
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (prettyDocU),
  )
import Prettyprinter (line, punctuate, vsep)
import Script (Script (Script))
import Term.Variable (Variable)

instance PrettyPrintableUnannotated 'Chick (Script α Variable) where
  prettyDocU (Script s) = vsep . punctuate line <$> mapM (prettyDocU @'Chick) s
