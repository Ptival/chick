{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module PrettyPrinting.Coq.Variable
  (
  ) where

import Text.PrettyPrint.Annotated.WL

import Language (Language(Coq))
import PrettyPrinting.PrettyPrintable
import Term.Variable

instance PrettyPrintable 'Coq Variable where
  prettyDoc  = text . unVariable
