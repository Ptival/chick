{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinting.Universe where

import Control.Monad.Reader
import Data.Default
import Text.PrettyPrint.Annotated.WL

import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import Term.Universe

instance PrettyPrintableUnannotated Universe where
  prettyDocU u = return $ text (show u)

instance PrettyPrintable Universe where
  prettyDoc t = runReader (prettyDocU t) def
  prettyStr = prettyStrU
