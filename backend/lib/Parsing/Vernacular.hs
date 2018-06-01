{-# language RankNTypes #-}

module Parsing.Vernacular
  ( definitionP
  , vernacularP
  ) where

import           Control.Applicative

import qualified Definition as D
import qualified DefinitionObjectKind as DOK
import           Parsing
import           Parsing.Types
import           Parsing.Chick.Utils
import           Parsing.Inductive
import           Term.Raw as Raw
import           Term.Term
import           Vernacular

vernacularP :: Parser (Vernacular Raw.Raw Variable)
vernacularP =
  definitionP
  <|> Inductive <$> (inductiveP <* symbol ".")

definitionP :: Parser (Vernacular Raw.Raw Variable)
definitionP = do
  k <- (rword "Definition" *> return DOK.Definition
        <|> rword "Fixpoint" *> return DOK.Fixpoint)
  n <- variableP
  symbol ":"
  τ <- termP
  symbol ":="
  t <- termP
  symbol "."
  return $ Definition (D.Definition k n τ t)
