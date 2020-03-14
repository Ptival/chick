{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module PrettyPrinting.Chick.Unit where

import Data.Text.Prettyprint.Doc ()

import Language (Language(Chick))
import PrettyPrinting.Chick.GlobalDeclaration ()
import PrettyPrinting.Chick.Inductive ()
import PrettyPrinting.Chick.Term ()
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated

instance PrettyPrintableUnannotated 'Chick () where
  prettyDocU = return . prettyDoc @'Chick

instance PrettyPrintable 'Chick () where
  prettyDoc () = "()"
