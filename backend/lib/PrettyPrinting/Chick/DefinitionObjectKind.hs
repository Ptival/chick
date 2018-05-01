{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module PrettyPrinting.Chick.DefinitionObjectKind
  (
  ) where

import Control.Monad.Reader (runReader)
import Data.Default (def)
import Text.PrettyPrint.Annotated.WL (text)

import DefinitionObjectKind
import Language (Language(Chick))
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated

instance PrettyPrintable 'Chick DefinitionObjectKind where
  prettyDoc v = runReader (prettyDocU @'Chick v) def

instance PrettyPrintableUnannotated 'Chick DefinitionObjectKind where
  prettyDocU = \case
    Definition -> return $ text "Definition"
    Fixpoint   -> return $ text "Fixpoint"
