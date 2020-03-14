{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module PrettyPrinting.OCaml.Binder
  (
  ) where

import Text.PrettyPrint.Annotated.WL

import Language (Language(OCaml))
import PrettyPrinting.PrettyPrintable
import Term.Binder

instance PrettyPrintable 'OCaml ν => PrettyPrintable 'OCaml (Binder ν) where
  prettyDoc (Binder Nothing)  = text "_"
  prettyDoc (Binder (Just v)) = prettyDoc @'OCaml v
