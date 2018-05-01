{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module PrettyPrinting.Chick.Script
  (
  ) where

import Control.Monad.Reader
import Data.Default
import Text.PrettyPrint.Annotated.WL

import Definition
import DefinitionObjectKind (DefinitionObjectKind)
import Language (Language(Chick))
import PrettyPrinting.Chick.Declaration ()
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import Script
import Term.Term

instance PrettyPrintableUnannotated 'Chick (Script α Variable) where
  prettyDocU (Script s) = vsep . punctuate line <$> mapM (prettyDocU @'Chick) s
