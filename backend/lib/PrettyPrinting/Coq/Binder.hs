{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module PrettyPrinting.Coq.Binder where

import qualified Prettyprinter      as Doc

import           Language                       (Language(Coq))
import           PrettyPrinting.PrettyPrintable
import           Term.Binder

instance PrettyPrintable 'Coq ν => PrettyPrintable 'Coq (Binder ν) where
  prettyDoc (Binder Nothing)  = Doc.pretty "_"
  prettyDoc (Binder (Just v)) = prettyDoc @'Coq v
