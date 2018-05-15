{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module PrettyPrinting.Chick.Script
  (
  ) where

import Data.Text.Prettyprint.Doc

import Language (Language(Chick))
import PrettyPrinting.Chick.Vernacular ()
import PrettyPrinting.PrettyPrintableUnannotated
import Script
import Term.Term

instance PrettyPrintableUnannotated 'Chick (Script α Variable) where
  prettyDocU (Script s) = vsep . punctuate line <$> mapM (prettyDocU @'Chick) s
