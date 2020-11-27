{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinting.Chick.DefinitionObjectKind where

import Control.Monad.Reader (runReader)
import Data.Default (def)
import Prettyprinter ()

import DefinitionObjectKind
import Language (Language(Chick))
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated

instance PrettyPrintable 'Chick DefinitionObjectKind where
  prettyDoc v = runReader (prettyDocU @'Chick v) def

instance PrettyPrintableUnannotated 'Chick DefinitionObjectKind where
  prettyDocU = \case
    Definition -> return "Definition"
    Fixpoint   -> return "Fixpoint"
