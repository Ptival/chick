{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PrettyPrinting.Chick.Universe where

import Control.Monad.Reader
import Data.Default
import Prettyprinter

import Language (Language(Chick))
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import Term.Universe

instance PrettyPrintableUnannotated 'Chick Universe where
  prettyDocU u = return $ pretty (show u)

instance PrettyPrintable 'Chick Universe where
  prettyDoc t = runReader (prettyDocU @'Chick t) def
  prettyStr = prettyStrU @'Chick
