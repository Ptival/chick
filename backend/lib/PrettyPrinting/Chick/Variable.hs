{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module PrettyPrinting.Chick.Variable
  (
  ) where

import Text.PrettyPrint.Annotated.WL

import Language (Language(Chick))
import PrettyPrinting.PrettyPrintable
import Term.Variable

instance PrettyPrintable 'Chick Variable where
  prettyDoc = text . unVariable
