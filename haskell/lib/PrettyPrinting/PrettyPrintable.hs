{-# LANGUAGE FlexibleContexts #-}

module PrettyPrinting.PrettyPrintable
  ( PrettyPrintable(..)
  ) where

import Text.PrettyPrint.Annotated.WL

class PrettyPrintable t where
  prettyDoc :: t -> Doc a
  prettyStr :: t -> String
  prettyStr = display . renderPretty 1.0 80 . prettyDoc
