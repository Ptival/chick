{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PrettyPrinting.Chick.Script
  (
  ) where

import Prettyprinter

import Language (Language(Chick))
import PrettyPrinting.Chick.Vernacular ()
import PrettyPrinting.PrettyPrintableUnannotated
import Script
import Term.Term

instance PrettyPrintableUnannotated 'Chick (Script α Variable) where
  prettyDocU (Script s) = vsep . punctuate line <$> mapM (prettyDocU @'Chick) s
