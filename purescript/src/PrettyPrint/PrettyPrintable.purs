module PrettyPrint.PrettyPrintable where

import Prelude
import PrettyPrint.PrettyPrint (Doc, display, renderPretty)

class PrettyPrintable t where
  prettyDoc :: ∀ a. t -> Doc a
  prettyStr :: t -> String

prettyStrDefault :: ∀ t. PrettyPrintable t => t -> String
prettyStrDefault = display <<< renderPretty 1.0 80 <<< prettyDoc
