{-# language RankNTypes #-}

module Parsing.Vernacular
  ( definitionP
  , vernacularP
  ) where

import Control.Applicative
import Text.Megaparsec.String

import Parsing
import Parsing.Inductive
import Parsing.Utils
import Term.Raw as Raw
import Term.Term
import Vernacular

vernacularP :: Parser (Vernacular Raw.Raw Variable)
vernacularP =
  definitionP
  <|> Inductive <$> (inductiveP <* symbol ".")

definitionP :: Parser (Vernacular Raw.Raw Variable)
definitionP = do
  rword "Definition"
  n <- variableP
  symbol ":"
  τ <- termP
  symbol ":="
  t <- termP
  symbol "."
  return $ Definition n τ t
