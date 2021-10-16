module Parsing.Vernacular
  ( definitionP,
    vernacularP,
  )
where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Definition as D
import qualified DefinitionObjectKind as DOK
import Parsing (termP, variableP)
import Parsing.Chick.Utils (rword, symbol)
import Parsing.Inductive (inductiveP)
import Parsing.Types (Parser)
import Term.Raw as Raw (Raw)
import Term.Variable (Variable)
import Vernacular (Vernacular (Definition, Inductive))

vernacularP :: Parser (Vernacular Raw.Raw Variable)
vernacularP =
  definitionP
    <|> Inductive <$> (inductiveP <* symbol ".")

definitionP :: Parser (Vernacular Raw.Raw Variable)
definitionP = do
  k <-
    rword "Definition" $> DOK.Definition
      <|> rword "Fixpoint" $> DOK.Fixpoint
  n <- variableP
  symbol ":"
  τ <- termP
  symbol ":="
  t <- termP
  symbol "."
  return $ Definition (D.Definition k n τ t)
