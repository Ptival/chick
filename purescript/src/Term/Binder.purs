module Term.Binder where

import Data.Maybe (Maybe(..))
import PrettyPrint.PrettyPrint (text)
import PrettyPrint.PrettyPrintable (class PrettyPrintable, prettyDoc, prettyStrDefault)

ignoreSymbol :: String
ignoreSymbol = "_"

newtype Binder ν = Binder (Maybe ν)

unBinder :: ∀ ν. Binder ν -> Maybe ν
unBinder (Binder b) = b

instance prettyPrintableBinder :: PrettyPrintable ν => PrettyPrintable (Binder ν) where
  prettyDoc (Binder Nothing)  = text "_"
  prettyDoc (Binder (Just v)) = prettyDoc v
  prettyStr t = prettyStrDefault t
