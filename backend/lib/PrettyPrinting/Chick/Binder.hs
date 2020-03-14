{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinting.Chick.Binder
  (
  ) where

import Data.Text.Prettyprint.Doc ()

import Language (Language(Chick))
import PrettyPrinting.Chick.Variable ()
import PrettyPrinting.PrettyPrintable
import Term.Binder
import Term.Variable
import PrettyPrinting.PrettyPrintableUnannotated

instance PrettyPrintable 'Chick ν => PrettyPrintable 'Chick (Binder ν) where
  prettyDoc (Binder Nothing)  = "_"
  prettyDoc (Binder (Just v)) = prettyDoc @'Chick v

instance PrettyPrintableUnannotated 'Chick (Binder Variable) where
  prettyDocU d = return $ prettyDoc @'Chick d
