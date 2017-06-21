module PrettyPrint.PrettyPrintable1 where

import Prelude
import Control.Monad.Reader (runReader)
import Control.Monad.Reader.Class (class MonadReader)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Parsing.Precedence (PrecedenceTable, defaultPrecedenceTable)
import PrettyPrint.PrettyPrint (Doc, display, renderCompact)
import Term.Variable (Variable)

class PrettyPrintable1 t where
  prettyDoc1 :: ∀ m. MonadReader PrecedenceTable m => t Variable -> m (Doc Unit)
  prettyStr1 :: t Variable -> String

-- | renderPretty is too slow in PureScript, defaulting to renderCompact for now...

prettyStr1Default :: ∀ t. PrettyPrintable1 t => t Variable -> String
prettyStr1Default t =
  replaceAll (Pattern "\n") (Replacement " ") <<< display <<< renderCompact <<< runReader (prettyDoc1 t) $ defaultPrecedenceTable

-- prettyStr1Default :: ∀ t. PrettyPrintable1 t => t Variable -> String
-- prettyStr1Default t =
--   display <<< renderPretty 1.0 80 <<< runReader (prettyDoc1 t) $ defaultPrecedenceTable
