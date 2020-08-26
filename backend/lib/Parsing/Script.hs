module Parsing.Script
  ( scriptP
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char

import Parsing.Types
import Parsing.Vernacular
import Script
import Term.Raw as Raw
import Term.Term

scriptP :: Parser (Script Raw.Raw Variable)
scriptP = Script <$> (space *> many (vernacularP <* space))
