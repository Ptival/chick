{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module PrettyPrinting.OCaml.Variable
  (
  ) where

import Text.PrettyPrint.Annotated.WL

import Language (Language(OCaml))
import PrettyPrinting.PrettyPrintable
import Term.Variable

instance PrettyPrintable 'OCaml Variable where
  prettyDoc  = text . unVariable
