module Term.Variable where

import Prelude
import Data.Generic (class Generic, gEq)
import PrettyPrint.PrettyPrint (text)
import PrettyPrint.PrettyPrintable (class PrettyPrintable, prettyStrDefault)

newtype Variable = Variable String

unVariable :: Variable -> String
unVariable (Variable v) = v

derive instance genericVariable :: Generic Variable

instance eqVariable :: Eq Variable where
  eq = gEq

instance prettyPrintableVariable :: PrettyPrintable Variable where
  prettyDoc (Variable v) = text v
  prettyStr t = prettyStrDefault t

instance showVariable :: Show Variable where
  show (Variable v) = v
