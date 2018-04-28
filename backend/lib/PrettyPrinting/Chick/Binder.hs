{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module PrettyPrinting.Chick.Binder
  (
  ) where

import Text.PrettyPrint.Annotated.WL

import Language (Language(Chick))
import PrettyPrinting.PrettyPrintable
import Term.Binder

instance PrettyPrintable 'Chick ν => PrettyPrintable 'Chick (Binder ν) where
  prettyDoc (Binder Nothing)  = text "_"
  prettyDoc (Binder (Just v)) = prettyDoc @'Chick v
