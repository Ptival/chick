module PrettyPrinting.Variable where

import PrettyPrinting.Utils
import Text.PrettyPrint.Annotated.WL

import Term.Term

prettyVariable :: Variable -> String
prettyVariable v = doc2String $ prettyVariableDoc v

prettyVariableDoc :: Variable -> Doc a
prettyVariableDoc (Variable s) = text s
