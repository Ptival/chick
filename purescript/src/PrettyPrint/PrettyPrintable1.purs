module PrettyPrint.PrettyPrintable1 where

import Prelude
import Control.Monad.Reader (runReader)
import Control.Monad.Reader.Class (class MonadReader)
import Parsing.Precedence (PrecedenceTable, defaultPrecedenceTable)
import PrettyPrint.PrettyPrint (Doc, display, renderCompact, renderPretty)
import Term.Variable (Variable)

class PrettyPrintable1 t where
  prettyDoc1 :: ∀ m. MonadReader PrecedenceTable m => t Variable -> m (Doc Unit)
  prettyStr1 :: t Variable -> String

-- prettyStr1Default :: ∀ t. PrettyPrintable1 t => t Variable -> String
-- prettyStr1Default t =
--   display <<< renderCompact <<< runReader (prettyDoc1 t) $ defaultPrecedenceTable

prettyStr1Default :: ∀ t. PrettyPrintable1 t => t Variable -> String
prettyStr1Default t =
  display <<< renderPretty 1.0 80 <<< runReader (prettyDoc1 t) $ defaultPrecedenceTable
