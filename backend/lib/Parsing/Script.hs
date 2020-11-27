module Parsing.Script
  ( scriptP,
  )
where

import Parsing.Types (Parser)
import Parsing.Vernacular (vernacularP)
import Script (Script (Script))
import Term.Raw as Raw (Raw)
import Term.Term (Variable)
import Text.Megaparsec (many)
import Text.Megaparsec.Char (space)

scriptP :: Parser (Script Raw.Raw Variable)
scriptP = Script <$> (space *> many (vernacularP <* space))
