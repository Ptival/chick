{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Term where

import Control.Monad.Reader.Class

import Precedence
import PrettyPrinting.Chick.Term as PPChick
import PrettyPrinting.PrettyPrintableUnannotated
import PrettyPrinting.Utils
import Term.Term

instance PrettyPrintableUnannotated (TermX ξ) where
  prettyDocU t = do
    precs <- ask
    return $ par precs (PrecMin, TolerateEqual) . PPChick.prettyTermDocPrec precs $ t
