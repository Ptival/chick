{-# LANGUAGE FlexibleContexts #-}

module PrettyPrinting.PrettyPrintable
  ( PrettyPrintable(..)
  ) where

import Text.PrettyPrint.Annotated.WL

class PrettyPrintable t where
  prettyDoc :: t -> Doc ()
  prettyStr :: t -> String
  prettyStr = display . renderPretty 1.0 80 . prettyDoc
  preview :: t -> String
  preview t =
    let s = prettyStr t in
    let previewLength = 20 in
      if length s <= previewLength
      then s
      else (++ "...") . take 20 $ s

instance (PrettyPrintable l, PrettyPrintable r) => PrettyPrintable (l, r) where
  prettyDoc (l, r) = fillSep [lparen, prettyDoc l, comma, prettyDoc r, rparen]
