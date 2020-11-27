{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Chick.Binder
  (
  )
where

import PrettyPrinting.Chick.Variable ()
import PrettyPrinting.HasNonBindingPattern
  ( HasNonBindingPattern (..),
  )
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyDoc),
  )
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (prettyDocU),
  )
import Prettyprinter (Pretty (pretty))
import Term.Binder (Binder (Binder))
import Term.Variable (Variable)

instance
  forall l v.
  ( HasNonBindingPattern l,
    PrettyPrintable l v
  ) =>
  PrettyPrintable l (Binder v)
  where
  prettyDoc (Binder Nothing) = pretty (nonBindingPattern @l)
  prettyDoc (Binder (Just v)) = prettyDoc @l v

instance
  ( HasNonBindingPattern l,
    PrettyPrintable l Variable
  ) =>
  PrettyPrintableUnannotated l (Binder Variable)
  where
  prettyDocU d = return $ prettyDoc @l d
