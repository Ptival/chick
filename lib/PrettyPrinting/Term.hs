{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinting.Term where

import Control.Monad.Reader
import Data.Default

import Precedence
import PrettyPrinting.Chick.Term as PPChick
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import PrettyPrinting.Utils
import Term.Term
import Term.Variable

instance PrettyPrintableUnannotated (TermX ξ Variable) where
  prettyDocU t = do
    precs <- ask
    return $ par precs (PrecMin, TolerateEqual) . PPChick.prettyTermDocPrec precs $ t

instance PrettyPrintable (TermX ξ Variable) where
  prettyDoc t = runReader (prettyDocU t) def
  prettyStr = prettyStrU
