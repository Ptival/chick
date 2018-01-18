{-# language RankNTypes #-}

module Parsing.Script
  ( scriptP
  ) where

import Control.Applicative
import Text.Megaparsec
-- import Text.Megaparsec.Char
import Text.Megaparsec.String

import Parsing.Vernacular
import Script
import Term.Raw as Raw
import Term.Term

scriptP :: Parser (Script Raw.Raw Variable)
scriptP = Script <$> (space *> many (vernacularP <* space))
