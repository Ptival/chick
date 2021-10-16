{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Chick.DefinitionObjectKind where

import Control.Monad.Reader (runReader)
import Data.Default (def)
import DefinitionObjectKind (DefinitionObjectKind (..))
import Language (Language (Chick))
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyDoc),
  )
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (prettyDocU),
  )
import Prettyprinter ()

instance PrettyPrintable 'Chick DefinitionObjectKind where
  prettyDoc v = runReader (prettyDocU @'Chick v) def

instance PrettyPrintableUnannotated 'Chick DefinitionObjectKind where
  prettyDocU = \case
    Definition -> return "Definition"
    Fixpoint -> return "Fixpoint"
