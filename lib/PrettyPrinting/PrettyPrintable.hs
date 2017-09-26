{-# LANGUAGE FlexibleContexts #-}

module PrettyPrinting.PrettyPrintable
  ( PrettyPrintable(..)
  ) where

import Text.PrettyPrint.Annotated.WL

class PrettyPrintable t where
  prettyDoc :: t -> Doc ()
  prettyStr :: t -> String
  prettyStr = display . renderPretty 1.0 72 . prettyDoc
  preview :: t -> String
  preview t =
    let s = prettyStr t in
    let previewLength = 20 in
      if length s <= previewLength
      then s
      else (++ "...") . take 20 $ s

instance PrettyPrintable () where
  prettyDoc () = text "()"

instance (PrettyPrintable l, PrettyPrintable r) => PrettyPrintable (l, r) where
  prettyDoc (l, r) = encloseSep lparen rparen comma [prettyDoc l, prettyDoc r]

instance (PrettyPrintable a, PrettyPrintable b, PrettyPrintable c) =>
         PrettyPrintable (a, b, c) where
  prettyDoc (a, b, c) = encloseSep lparen rparen comma
    [prettyDoc a, prettyDoc b, prettyDoc c]

instance (PrettyPrintable a) => PrettyPrintable [a] where
  prettyDoc l = encloseSep lbracket rbracket comma (map prettyDoc l)

instance PrettyPrintable Int where
  prettyDoc i = text (show i)
