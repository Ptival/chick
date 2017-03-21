module PrettyPrinting.Binder where

import Text.PrettyPrint.Annotated.WL

import PrettyPrinting.Utils
import PrettyPrinting.Variable
import Term.Term

prettyBinder :: Binder -> String
prettyBinder b = doc2String $ prettyBinderDoc b

prettyBinderDoc :: Binder -> Doc a
prettyBinderDoc (Binder Nothing)  = text "_"
prettyBinderDoc (Binder (Just v)) = prettyVariableDoc v
