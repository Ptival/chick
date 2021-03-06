{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module PrettyPrinting.Chick.GlobalEnvironment where

import Control.Monad.Reader
import Data.Default
import Prettyprinter

import Language (Language(Chick))
import PrettyPrinting.Chick.GlobalDeclaration ()
import PrettyPrinting.Chick.Inductive ()
import PrettyPrinting.Chick.Term ()
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import Term.Term
import Typing.GlobalEnvironment

instance PrettyPrintableUnannotated 'Chick (GlobalEnvironment ξ Variable) where
  prettyDocU (GlobalEnvironment γ) =
    encloseSep lbracket rbracket comma <$> mapM (prettyDocU @'Chick) (reverse γ)

instance PrettyPrintable 'Chick (GlobalEnvironment ξ Variable) where
  prettyDoc γ = runReader (prettyDocU @'Chick γ) def
