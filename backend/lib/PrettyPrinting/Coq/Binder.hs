{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module PrettyPrinting.Coq.Binder
  (
  ) where

import Text.PrettyPrint.Annotated.WL

import Language (Language(Coq))
import PrettyPrinting.PrettyPrintable
import Term.Binder

instance PrettyPrintable 'Coq ν => PrettyPrintable 'Coq (Binder ν) where
  prettyDoc (Binder Nothing)  = text "_"
  prettyDoc (Binder (Just v)) = prettyDoc @'Coq v
